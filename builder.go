package karbuf

import (
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"strings"
)

type builtField struct {
	field *Field
	typ   ast.GenDecl
	enc   ast.FuncDecl
	dec   ast.FuncDecl
}

type Builder struct {
	cnt     uint
	pak     string
	imports *ast.GenDecl
	types   map[string]builtField
}

func NewBuilder() *Builder {
	return &Builder{
		types: make(map[string]builtField),
	}
}

func (e *Builder) encPrim(writer ast.Expr, ptr ast.Expr, s *Field) (stmts []ast.Stmt) {
	switch {
	case s.typ.IsPrimitive():
		stmts = append(stmts, newCallST(
			newSel(writer, "Copy"),
			unsafePtr(newPtr(ptr)),
			intLit(s.typ.Size()),
		))
	case s.typ.IsType(FieldSlice) || s.typ.IsType(FieldString):
		stmts = append(stmts,
			newCallST(
				newSel(writer, "WriteLen"),
				newLen(ptr),
			),
		)
		if s.sliceType.typ.IsPrimitive() {
			hdr := e.newIdent()
			var bstmts []ast.Stmt
			if s.typ.IsType(FieldString) {
				bstmts = append(bstmts,
					newDef(hdr, newCall(&ast.ParenExpr{X: &ast.UnaryExpr{X: newSel("reflect", "StringHeader"), Op: token.MUL}}, unsafePtr(newPtr(ptr)))),
				)
			} else {
				bstmts = append(bstmts,
					newDef(hdr, newCall(&ast.ParenExpr{X: &ast.UnaryExpr{X: newSel("reflect", "SliceHeader"), Op: token.MUL}}, unsafePtr(newPtr(ptr)))),
				)
			}
			bstmts = append(bstmts,
				newCallST(
					newSel(writer, "Copy"),
					unsafePtr(newSel(hdr, "Data")),
					newMul(intLit(s.sliceType.typ.Size()), newLen(ptr)),
				),
			)
			stmts = append(stmts, &ast.IfStmt{
				Cond: &ast.BinaryExpr{X: newLen(ptr), Op: token.GTR, Y: intLit(0)},
				Body: &ast.BlockStmt{List: bstmts},
			})
		} else {
			i := e.newIdent()
			stmts = append(stmts, &ast.RangeStmt{
				Key:  i,
				Tok:  token.DEFINE,
				X:    ptr,
				Body: &ast.BlockStmt{List: e.encField(writer, newIdx(ptr, i), s.sliceType)},
			})
		}
	case s.typ.IsType(FieldStruct):
		for i := range s.strucFields {
			stmts = append(stmts, e.encField(writer, newSel(ptr, s.strucFields[i].strucName), s.strucFields[i].Field)...)
		}
	default:
		panic("wth")
	}
	return
}

func (e *Builder) encField(writer ast.Expr, ptr ast.Expr, s *Field) (stmts []ast.Stmt) {
	if _, ok := e.types[s.typename]; ok {
		stmts = append(stmts, newCallST(
			newSel(ptr, "Encode"),
			writer,
		))
		return
	}

	stmts = e.encPrim(writer, ptr, s)
	return
}

func (e *Builder) decPrim(reader ast.Expr, ptr ast.Expr, s *Field) (stmts []ast.Stmt) {
	switch {
	case s.typ.IsPrimitive():
		stmts = append(stmts, newCallST(
			newSel(reader, "Copy"),
			unsafePtr(newPtr(ptr)),
			intLit(s.typ.Size()),
		))
	case s.typ.IsType(FieldSlice) || s.typ.IsType(FieldString):
		length := e.newIdent()
		stmts = append(stmts, newDef(length, newCall(newSel(reader, "ReadLen"))))
		if s.sliceType.typ.IsPrimitive() {
			hdr := e.newIdent()
			var bstmts []ast.Stmt
			if s.typ.IsType(FieldSlice) {
				bstmts = append(bstmts,
					newDef(hdr, newCall(&ast.ParenExpr{X: &ast.UnaryExpr{X: newSel("reflect", "SliceHeader"), Op: token.MUL}}, unsafePtr(newPtr(ptr)))),
				)
			} else {
				bstmts = append(bstmts,
					newDef(hdr, newCall(&ast.ParenExpr{X: &ast.UnaryExpr{X: newSel("reflect", "StringHeader"), Op: token.MUL}}, unsafePtr(newPtr(ptr)))),
				)
			}
			bstmts = append(bstmts,
				newAssign(newSel(hdr, "Len"), length),
				newAssign(newSel(hdr, "Data"), newCall(
					newSel(reader, "Read"),
					newMul(intLit(s.sliceType.typ.Size()), length),
				)),
			)
			if s.typ.IsType(FieldSlice) {
				bstmts = append(bstmts,
					newAssign(newSel(hdr, "Cap"), length),
				)
			}
			stmts = append(stmts, &ast.IfStmt{
				Cond: &ast.BinaryExpr{X: length, Op: token.GTR, Y: intLit(0)},
				Body: &ast.BlockStmt{List: bstmts},
			})
		} else {
			i := e.newIdent()
			stmts = append(stmts,
				newAssign(ptr, newCall("make", &ast.ArrayType{Elt: e.typWrap(s.sliceType)}, length)),
				&ast.RangeStmt{
					Key:  newIdent(i),
					Tok:  token.DEFINE,
					X:    ptr,
					Body: &ast.BlockStmt{List: e.decField(reader, newIdx(ptr, i), s.sliceType)},
				},
			)
		}
	case s.typ.IsType(FieldStruct):
		for i := range s.strucFields {
			stmts = append(stmts, e.decField(reader, newSel(ptr, s.strucFields[i].strucName), s.strucFields[i].Field)...)
		}
	default:
		panic("wth")
	}
	return
}

func (e *Builder) decField(reader, ptr ast.Expr, s *Field) (stmts []ast.Stmt) {
	if _, ok := e.types[s.typename]; ok {
		stmts = append(stmts, newCallST(
			newSel(ptr, "Decode"),
			reader,
		))
		return
	}

	stmts = e.decPrim(reader, ptr, s)
	return
}

func (e *Builder) newIdent() *ast.Ident {
	r := ast.NewIdent(fmt.Sprintf("v%d", e.cnt))
	e.cnt++
	return r
}

func (e *Builder) typPrim(s *Field) ast.Expr {
	switch {
	case s.typ.IsPrimitive() || s.typ.IsType(FieldString):
		return newIdent(s.typ.String())
	case s.typ.IsType(FieldSlice):
		return &ast.ArrayType{
			Elt: e.typWrap(s.sliceType),
		}
	case s.typ.IsType(FieldStruct):
		var fields []*ast.Field
		for _, field := range s.strucFields {
			fields = append(fields, &ast.Field{
				Names: []*ast.Ident{ast.NewIdent(field.strucName)},
				Type:  e.typWrap(field.Field),
			})
		}
		return &ast.StructType{Fields: &ast.FieldList{List: fields}}
	default:
		return newIdent("invalid")
	}
}

func (e *Builder) typWrap(s *Field) ast.Expr {
	if _, ok := e.types[s.typename]; ok {
		return newIdent(s.typename)
	}

	return e.typPrim(s)
}

func (e *Builder) Process() {
	e.cnt = 0

	e.pak = "bench"

	e.imports = &ast.GenDecl{
		Tok: token.IMPORT,
	}
	e.imports.Specs = append(e.imports.Specs,
		&ast.ImportSpec{
			Path: &ast.BasicLit{
				Kind:  token.STRING,
				Value: "\"unsafe\"",
			},
		},
		&ast.ImportSpec{
			Path: &ast.BasicLit{
				Kind:  token.STRING,
				Value: "\"reflect\"",
			},
		},
		&ast.ImportSpec{
			Path: &ast.BasicLit{
				Kind:  token.STRING,
				Value: "\"github.com/xhebox/karbuf\"",
			},
		},
	)

	for name, el := range e.types {
		val := ast.NewIdent("v")

		el.typ = ast.GenDecl{
			Tok: token.TYPE,
			Specs: []ast.Spec{
				&ast.TypeSpec{
					Name: ast.NewIdent(name),
					Type: e.typPrim(el.field),
				},
			},
		}

		writer := ast.NewIdent("wt")
		typ := newIdent(el.field.typename)
		if t := el.field.typ; t.IsType(FieldStruct) {
			typ = &ast.UnaryExpr{X: typ, Op: token.MUL}
		}
		el.enc = ast.FuncDecl{
			Name: ast.NewIdent("Encode"),
			Recv: &ast.FieldList{List: []*ast.Field{
				{Names: []*ast.Ident{val}, Type: typ},
			}},
			Type: &ast.FuncType{
				Params: &ast.FieldList{List: []*ast.Field{
					{Names: []*ast.Ident{writer}, Type: writerType},
				}},
				Results: &ast.FieldList{List: []*ast.Field{}},
			},
			Body: &ast.BlockStmt{List: e.encPrim(writer, val, el.field)},
		}

		reader := ast.NewIdent("rd")
		el.dec = ast.FuncDecl{
			Name: ast.NewIdent("Decode"),
			Recv: &ast.FieldList{List: []*ast.Field{
				{Names: []*ast.Ident{val}, Type: typ},
			}},
			Type: &ast.FuncType{
				Params: &ast.FieldList{List: []*ast.Field{
					{Names: []*ast.Ident{reader}, Type: readerType},
				}},
				Results: &ast.FieldList{List: []*ast.Field{}},
			},
			Body: &ast.BlockStmt{List: e.decPrim(reader, val, el.field)},
		}

		e.types[name] = el
	}
}

func (e *Builder) Print(buf *strings.Builder) error {
	ts := token.NewFileSet()
	cfg := printer.Config{
		Mode:     printer.TabIndent,
		Tabwidth: 2,
	}
	file := &ast.File{
		Name:  ast.NewIdent(e.pak),
		Decls: []ast.Decl{e.imports},
	}
	for _, e := range e.types {
		el := e
		file.Decls = append(file.Decls, &el.typ, &el.enc, &el.dec)
	}
	if err := cfg.Fprint(buf, ts, file); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(buf, "\n"); err != nil {
		return err
	}
	return nil
}

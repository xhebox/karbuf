package karbuf

import (
	"go/ast"
	"io"
	"sort"
)

const maxver = ^uint(0)

type state struct {
	size uint
}

type Struct struct {
	ver    uint
	fields []Field
}

func (s *Struct) allVersions() []uint {
	verm := make(map[uint]struct{})
	/*
		for _, f := range s.fields {
			verm[f.vers] = struct{}{}
			verm[f.vere] = struct{}{}
		}
	*/
	vers := make([]uint, len(verm))
	for v := range verm {
		vers = append(vers, v)
	}
	sort.Slice(vers, func(i, j int) bool {
		return vers[i] < vers[j]
	})
	if i := sort.Search(len(vers), func(i int) bool {
		return vers[i] >= s.ver
	}); i <= len(vers) {
		vers = vers[i:]
	}
	return vers
}

func (s *Struct) gen(w io.Writer) error {
	vers := s.allVersions()
	if len(vers) == 0 {
		panic("should not be")
	}

	decls := make([]*ast.DeclStmt, 0, len(vers)*2+3)
	decls = append(decls, s.genTypedef())
	for _, ver := range vers {
		decls = append(decls, s.genEncode(ver), s.genDecode(ver))
	}

	if err := ast.Fprint(w, nil, decls, nil); err != nil {
		return err
	}

	return nil
}

func (s *Struct) genTypedef() *ast.DeclStmt {
	return nil
}

func (s *Struct) genEncode(ver uint) *ast.DeclStmt {
	return nil
}

func (s *Struct) genDecode(ver uint) *ast.DeclStmt {
	return nil
}

func (s *Struct) AppendFields(fields ...*Field) *Struct {
	for _, v := range fields {
		s.fields = append(s.fields, *v)
	}
	return s
}

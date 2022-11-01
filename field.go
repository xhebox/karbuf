package karbuf

import ()

type FieldType uint

const (
	FieldInvalid FieldType = iota
	FieldBool
	FieldInt8
	FieldInt16
	FieldInt32
	FieldInt64
	FieldUint8
	FieldUint16
	FieldUint32
	FieldUint64
	FieldFloat32
	FieldFloat64
	FieldString
	FieldSlice
	FieldStruct
)

func (ft FieldType) IsPrimitive() bool {
	switch ft {
	case FieldBool, FieldFloat32, FieldFloat64, FieldInt8, FieldInt16, FieldInt32, FieldInt64, FieldUint8, FieldUint16, FieldUint32, FieldUint64:
		return true
	default:
		return false
	}
}

func (ft FieldType) String() string {
	switch ft {
	case FieldBool:
		return "bool"
	case FieldInt8:
		return "int8"
	case FieldInt16:
		return "int16"
	case FieldInt32:
		return "int32"
	case FieldInt64:
		return "int64"
	case FieldUint8:
		return "uint8"
	case FieldUint16:
		return "uint16"
	case FieldUint32:
		return "uint32"
	case FieldUint64:
		return "uint64"
	case FieldFloat32:
		return "float32"
	case FieldFloat64:
		return "float64"
	case FieldString:
		return "string"
	case FieldSlice:
		return "slice"
	case FieldStruct:
		return "struct"
	default:
		return "invalid"
	}
}

func (ft FieldType) Size() uint {
	sz := uint(0)
	switch ft {
	case FieldBool, FieldInt8, FieldUint8:
		sz = 1
	case FieldInt16, FieldUint16:
		sz = 2
	case FieldInt32, FieldUint32, FieldFloat32:
		sz = 4
	case FieldInt64, FieldUint64, FieldFloat64:
		sz = 8
	}
	return sz
}

func (ft FieldType) IsType(t FieldType) bool {
	return ft == t
}

type StructField struct {
	*Field
	strucName string
}

type Field struct {
	// all
	typename string
	typ      FieldType
	virtual  bool
	// FieldSlice
	sliceType *Field
	// FieldStruct
	strucFields []StructField
}

func New(typ FieldType) *Field {
	if typ.IsType(FieldSlice) || typ.IsType(FieldString) {
		panic("should not pass slice or string type")
	}
	return &Field{
		typ: typ,
	}
}

func NewSlice(t *Field) *Field {
	return &Field{
		typ:       FieldSlice,
		sliceType: t,
	}
}

func NewString() *Field {
	return &Field{
		typ:       FieldString,
		sliceType: New(FieldUint8),
	}
}

func (b *Field) Add(name string, field *Field) *Field {
	b.strucFields = append(b.strucFields, StructField{
		strucName: name,
		Field:     field,
	})
	return b
}

func (s *Field) Virtual() *Field {
	s.virtual = true
	return s
}

func (s *Field) Reg(e *Builder, name string) *Field {
	s.typename = name
	e.types[name] = builtField{field: s}
	return s
}

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/*
 * Runtime type representation.
 * This file exists only to provide types that 6l can turn into
 * DWARF information for use by gdb.  Nothing else uses these.
 * They should match the same types in ../reflect/type.go.
 * For comments see ../reflect/type.go.
 */

package runtime

import "unsafe"

type rtype struct {
	Kind       uint8
	align      uint8
	fieldAlign uint8
	size       uintptr
	hash       uint32

	hashfn  func(unsafe.Pointer, uintptr) uintptr
	equalfn func(unsafe.Pointer, unsafe.Pointer, uintptr) bool

	string *string
	*uncommonType
	ptrToThis *rtype
}

type _method struct {
	name    *string
	pkgPath *string
	mtyp    *rtype
	typ     *rtype
	tfn     unsafe.Pointer
}

type uncommonType struct {
	name    *string
	pkgPath *string
	methods []_method
}

type _imethod struct {
	name    *string
	pkgPath *string
	typ     *rtype
}

type interfaceType struct {
	rtype
	methods []_imethod
}

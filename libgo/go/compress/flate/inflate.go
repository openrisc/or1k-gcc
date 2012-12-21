// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package flate implements the DEFLATE compressed data format, described in
// RFC 1951.  The gzip and zlib packages implement access to DEFLATE-based file
// formats.
package flate

import (
	"bufio"
	"io"
	"strconv"
)

const (
	maxCodeLen = 16    // max length of Huffman code
	maxHist    = 32768 // max history required
	// The next three numbers come from the RFC, section 3.2.7.
	maxLit   = 286
	maxDist  = 32
	numCodes = 19 // number of codes in Huffman meta-code
)

// A CorruptInputError reports the presence of corrupt input at a given offset.
type CorruptInputError int64

func (e CorruptInputError) Error() string {
	return "flate: corrupt input before offset " + strconv.FormatInt(int64(e), 10)
}

// An InternalError reports an error in the flate code itself.
type InternalError string

func (e InternalError) Error() string { return "flate: internal error: " + string(e) }

// A ReadError reports an error encountered while reading input.
type ReadError struct {
	Offset int64 // byte offset where error occurred
	Err    error // error returned by underlying Read
}

func (e *ReadError) Error() string {
	return "flate: read error at offset " + strconv.FormatInt(e.Offset, 10) + ": " + e.Err.Error()
}

// A WriteError reports an error encountered while writing output.
type WriteError struct {
	Offset int64 // byte offset where error occurred
	Err    error // error returned by underlying Write
}

func (e *WriteError) Error() string {
	return "flate: write error at offset " + strconv.FormatInt(e.Offset, 10) + ": " + e.Err.Error()
}

// Huffman decoder is based on
// J. Brian Connell, ``A Huffman-Shannon-Fano Code,''
// Proceedings of the IEEE, 61(7) (July 1973), pp 1046-1047.
type huffmanDecoder struct {
	// min, max code length
	min, max int

	// limit[i] = largest code word of length i
	// Given code v of length n,
	// need more bits if v > limit[n].
	limit [maxCodeLen + 1]int

	// base[i] = smallest code word of length i - seq number
	base [maxCodeLen + 1]int

	// codes[seq number] = output code.
	// Given code v of length n, value is
	// codes[v - base[n]].
	codes []int
}

// Initialize Huffman decoding tables from array of code lengths.
func (h *huffmanDecoder) init(bits []int) bool {
	// Count number of codes of each length,
	// compute min and max length.
	var count [maxCodeLen + 1]int
	var min, max int
	for _, n := range bits {
		if n == 0 {
			continue
		}
		if min == 0 || n < min {
			min = n
		}
		if n > max {
			max = n
		}
		count[n]++
	}
	if max == 0 {
		return false
	}

	h.min = min
	h.max = max

	// For each code range, compute
	// nextcode (first code of that length),
	// limit (last code of that length), and
	// base (offset from first code to sequence number).
	code := 0
	seq := 0
	var nextcode [maxCodeLen]int
	for i := min; i <= max; i++ {
		n := count[i]
		nextcode[i] = code
		h.base[i] = code - seq
		code += n
		seq += n
		h.limit[i] = code - 1
		code <<= 1
	}

	// Make array mapping sequence numbers to codes.
	if len(h.codes) < len(bits) {
		h.codes = make([]int, len(bits))
	}
	for i, n := range bits {
		if n == 0 {
			continue
		}
		code := nextcode[n]
		nextcode[n]++
		seq := code - h.base[n]
		h.codes[seq] = i
	}
	return true
}

// Hard-coded Huffman tables for DEFLATE algorithm.
// See RFC 1951, section 3.2.6.
var fixedHuffmanDecoder = huffmanDecoder{
	7, 9,
	[maxCodeLen + 1]int{7: 23, 199, 511},
	[maxCodeLen + 1]int{7: 0, 24, 224},
	[]int{
		// length 7: 256-279
		256, 257, 258, 259, 260, 261, 262,
		263, 264, 265, 266, 267, 268, 269,
		270, 271, 272, 273, 274, 275, 276,
		277, 278, 279,

		// length 8: 0-143
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
		12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
		22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
		32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
		42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
		52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
		62, 63, 64, 65, 66, 67, 68, 69, 70, 71,
		72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
		82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
		92, 93, 94, 95, 96, 97, 98, 99, 100,
		101, 102, 103, 104, 105, 106, 107, 108,
		109, 110, 111, 112, 113, 114, 115, 116,
		117, 118, 119, 120, 121, 122, 123, 124,
		125, 126, 127, 128, 129, 130, 131, 132,
		133, 134, 135, 136, 137, 138, 139, 140,
		141, 142, 143,

		// length 8: 280-287
		280, 281, 282, 283, 284, 285, 286, 287,

		// length 9: 144-255
		144, 145, 146, 147, 148, 149, 150, 151,
		152, 153, 154, 155, 156, 157, 158, 159,
		160, 161, 162, 163, 164, 165, 166, 167,
		168, 169, 170, 171, 172, 173, 174, 175,
		176, 177, 178, 179, 180, 181, 182, 183,
		184, 185, 186, 187, 188, 189, 190, 191,
		192, 193, 194, 195, 196, 197, 198, 199,
		200, 201, 202, 203, 204, 205, 206, 207,
		208, 209, 210, 211, 212, 213, 214, 215,
		216, 217, 218, 219, 220, 221, 222, 223,
		224, 225, 226, 227, 228, 229, 230, 231,
		232, 233, 234, 235, 236, 237, 238, 239,
		240, 241, 242, 243, 244, 245, 246, 247,
		248, 249, 250, 251, 252, 253, 254, 255,
	},
}

// The actual read interface needed by NewReader.
// If the passed in io.Reader does not also have ReadByte,
// the NewReader will introduce its own buffering.
type Reader interface {
	io.Reader
	ReadByte() (c byte, err error)
}

// Decompress state.
type decompressor struct {
	// Input source.
	r       Reader
	roffset int64
	woffset int64

	// Input bits, in top of b.
	b  uint32
	nb uint

	// Huffman decoders for literal/length, distance.
	h1, h2 huffmanDecoder

	// Length arrays used to define Huffman codes.
	bits     *[maxLit + maxDist]int
	codebits *[numCodes]int

	// Output history, buffer.
	hist  *[maxHist]byte
	hp    int  // current output position in buffer
	hw    int  // have written hist[0:hw] already
	hfull bool // buffer has filled at least once

	// Temporary buffer (avoids repeated allocation).
	buf [4]byte

	// Next step in the decompression,
	// and decompression state.
	step     func(*decompressor)
	final    bool
	err      error
	toRead   []byte
	hl, hd   *huffmanDecoder
	copyLen  int
	copyDist int
}

func (f *decompressor) nextBlock() {
	if f.final {
		if f.hw != f.hp {
			f.flush((*decompressor).nextBlock)
			return
		}
		f.err = io.EOF
		return
	}
	for f.nb < 1+2 {
		if f.err = f.moreBits(); f.err != nil {
			return
		}
	}
	f.final = f.b&1 == 1
	f.b >>= 1
	typ := f.b & 3
	f.b >>= 2
	f.nb -= 1 + 2
	switch typ {
	case 0:
		f.dataBlock()
	case 1:
		// compressed, fixed Huffman tables
		f.hl = &fixedHuffmanDecoder
		f.hd = nil
		f.huffmanBlock()
	case 2:
		// compressed, dynamic Huffman tables
		if f.err = f.readHuffman(); f.err != nil {
			break
		}
		f.hl = &f.h1
		f.hd = &f.h2
		f.huffmanBlock()
	default:
		// 3 is reserved.
		f.err = CorruptInputError(f.roffset)
	}
}

func (f *decompressor) Read(b []byte) (int, error) {
	for {
		if len(f.toRead) > 0 {
			n := copy(b, f.toRead)
			f.toRead = f.toRead[n:]
			return n, nil
		}
		if f.err != nil {
			return 0, f.err
		}
		f.step(f)
	}
	panic("unreachable")
}

func (f *decompressor) Close() error {
	if f.err == io.EOF {
		return nil
	}
	return f.err
}

// RFC 1951 section 3.2.7.
// Compression with dynamic Huffman codes

var codeOrder = [...]int{16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15}

func (f *decompressor) readHuffman() error {
	// HLIT[5], HDIST[5], HCLEN[4].
	for f.nb < 5+5+4 {
		if err := f.moreBits(); err != nil {
			return err
		}
	}
	nlit := int(f.b&0x1F) + 257
	if nlit > maxLit {
		return CorruptInputError(f.roffset)
	}
	f.b >>= 5
	ndist := int(f.b&0x1F) + 1
	// maxDist is 32, so ndist is always valid.
	f.b >>= 5
	nclen := int(f.b&0xF) + 4
	// numCodes is 19, so nclen is always valid.
	f.b >>= 4
	f.nb -= 5 + 5 + 4

	// (HCLEN+4)*3 bits: code lengths in the magic codeOrder order.
	for i := 0; i < nclen; i++ {
		for f.nb < 3 {
			if err := f.moreBits(); err != nil {
				return err
			}
		}
		f.codebits[codeOrder[i]] = int(f.b & 0x7)
		f.b >>= 3
		f.nb -= 3
	}
	for i := nclen; i < len(codeOrder); i++ {
		f.codebits[codeOrder[i]] = 0
	}
	if !f.h1.init(f.codebits[0:]) {
		return CorruptInputError(f.roffset)
	}

	// HLIT + 257 code lengths, HDIST + 1 code lengths,
	// using the code length Huffman code.
	for i, n := 0, nlit+ndist; i < n; {
		x, err := f.huffSym(&f.h1)
		if err != nil {
			return err
		}
		if x < 16 {
			// Actual length.
			f.bits[i] = x
			i++
			continue
		}
		// Repeat previous length or zero.
		var rep int
		var nb uint
		var b int
		switch x {
		default:
			return InternalError("unexpected length code")
		case 16:
			rep = 3
			nb = 2
			if i == 0 {
				return CorruptInputError(f.roffset)
			}
			b = f.bits[i-1]
		case 17:
			rep = 3
			nb = 3
			b = 0
		case 18:
			rep = 11
			nb = 7
			b = 0
		}
		for f.nb < nb {
			if err := f.moreBits(); err != nil {
				return err
			}
		}
		rep += int(f.b & uint32(1<<nb-1))
		f.b >>= nb
		f.nb -= nb
		if i+rep > n {
			return CorruptInputError(f.roffset)
		}
		for j := 0; j < rep; j++ {
			f.bits[i] = b
			i++
		}
	}

	if !f.h1.init(f.bits[0:nlit]) || !f.h2.init(f.bits[nlit:nlit+ndist]) {
		return CorruptInputError(f.roffset)
	}

	return nil
}

// Decode a single Huffman block from f.
// hl and hd are the Huffman states for the lit/length values
// and the distance values, respectively.  If hd == nil, using the
// fixed distance encoding associated with fixed Huffman blocks.
func (f *decompressor) huffmanBlock() {
	for {
		v, err := f.huffSym(f.hl)
		if err != nil {
			f.err = err
			return
		}
		var n uint // number of bits extra
		var length int
		switch {
		case v < 256:
			f.hist[f.hp] = byte(v)
			f.hp++
			if f.hp == len(f.hist) {
				// After the flush, continue this loop.
				f.flush((*decompressor).huffmanBlock)
				return
			}
			continue
		case v == 256:
			// Done with huffman block; read next block.
			f.step = (*decompressor).nextBlock
			return
		// otherwise, reference to older data
		case v < 265:
			length = v - (257 - 3)
			n = 0
		case v < 269:
			length = v*2 - (265*2 - 11)
			n = 1
		case v < 273:
			length = v*4 - (269*4 - 19)
			n = 2
		case v < 277:
			length = v*8 - (273*8 - 35)
			n = 3
		case v < 281:
			length = v*16 - (277*16 - 67)
			n = 4
		case v < 285:
			length = v*32 - (281*32 - 131)
			n = 5
		default:
			length = 258
			n = 0
		}
		if n > 0 {
			for f.nb < n {
				if err = f.moreBits(); err != nil {
					f.err = err
					return
				}
			}
			length += int(f.b & uint32(1<<n-1))
			f.b >>= n
			f.nb -= n
		}

		var dist int
		if f.hd == nil {
			for f.nb < 5 {
				if err = f.moreBits(); err != nil {
					f.err = err
					return
				}
			}
			dist = int(reverseByte[(f.b&0x1F)<<3])
			f.b >>= 5
			f.nb -= 5
		} else {
			if dist, err = f.huffSym(f.hd); err != nil {
				f.err = err
				return
			}
		}

		switch {
		case dist < 4:
			dist++
		case dist >= 30:
			f.err = CorruptInputError(f.roffset)
			return
		default:
			nb := uint(dist-2) >> 1
			// have 1 bit in bottom of dist, need nb more.
			extra := (dist & 1) << nb
			for f.nb < nb {
				if err = f.moreBits(); err != nil {
					f.err = err
					return
				}
			}
			extra |= int(f.b & uint32(1<<nb-1))
			f.b >>= nb
			f.nb -= nb
			dist = 1<<(nb+1) + 1 + extra
		}

		// Copy history[-dist:-dist+length] into output.
		if dist > len(f.hist) {
			f.err = InternalError("bad history distance")
			return
		}

		// No check on length; encoding can be prescient.
		if !f.hfull && dist > f.hp {
			f.err = CorruptInputError(f.roffset)
			return
		}

		f.copyLen, f.copyDist = length, dist
		if f.copyHist() {
			return
		}
	}
	panic("unreached")
}

// copyHist copies f.copyLen bytes from f.hist (f.copyDist bytes ago) to itself.
// It reports whether the f.hist buffer is full.
func (f *decompressor) copyHist() bool {
	p := f.hp - f.copyDist
	if p < 0 {
		p += len(f.hist)
	}
	for f.copyLen > 0 {
		n := f.copyLen
		if x := len(f.hist) - f.hp; n > x {
			n = x
		}
		if x := len(f.hist) - p; n > x {
			n = x
		}
		forwardCopy(f.hist[f.hp:f.hp+n], f.hist[p:p+n])
		p += n
		f.hp += n
		f.copyLen -= n
		if f.hp == len(f.hist) {
			// After flush continue copying out of history.
			f.flush((*decompressor).copyHuff)
			return true
		}
		if p == len(f.hist) {
			p = 0
		}
	}
	return false
}

func (f *decompressor) copyHuff() {
	if f.copyHist() {
		return
	}
	f.huffmanBlock()
}

// Copy a single uncompressed data block from input to output.
func (f *decompressor) dataBlock() {
	// Uncompressed.
	// Discard current half-byte.
	f.nb = 0
	f.b = 0

	// Length then ones-complement of length.
	nr, err := io.ReadFull(f.r, f.buf[0:4])
	f.roffset += int64(nr)
	if err != nil {
		f.err = &ReadError{f.roffset, err}
		return
	}
	n := int(f.buf[0]) | int(f.buf[1])<<8
	nn := int(f.buf[2]) | int(f.buf[3])<<8
	if uint16(nn) != uint16(^n) {
		f.err = CorruptInputError(f.roffset)
		return
	}

	if n == 0 {
		// 0-length block means sync
		f.flush((*decompressor).nextBlock)
		return
	}

	f.copyLen = n
	f.copyData()
}

// copyData copies f.copyLen bytes from the underlying reader into f.hist.
// It pauses for reads when f.hist is full.
func (f *decompressor) copyData() {
	n := f.copyLen
	for n > 0 {
		m := len(f.hist) - f.hp
		if m > n {
			m = n
		}
		m, err := io.ReadFull(f.r, f.hist[f.hp:f.hp+m])
		f.roffset += int64(m)
		if err != nil {
			f.err = &ReadError{f.roffset, err}
			return
		}
		n -= m
		f.hp += m
		if f.hp == len(f.hist) {
			f.copyLen = n
			f.flush((*decompressor).copyData)
			return
		}
	}
	f.step = (*decompressor).nextBlock
}

func (f *decompressor) setDict(dict []byte) {
	if len(dict) > len(f.hist) {
		// Will only remember the tail.
		dict = dict[len(dict)-len(f.hist):]
	}

	f.hp = copy(f.hist[:], dict)
	if f.hp == len(f.hist) {
		f.hp = 0
		f.hfull = true
	}
	f.hw = f.hp
}

func (f *decompressor) moreBits() error {
	c, err := f.r.ReadByte()
	if err != nil {
		if err == io.EOF {
			err = io.ErrUnexpectedEOF
		}
		return err
	}
	f.roffset++
	f.b |= uint32(c) << f.nb
	f.nb += 8
	return nil
}

// Read the next Huffman-encoded symbol from f according to h.
func (f *decompressor) huffSym(h *huffmanDecoder) (int, error) {
	for n := uint(h.min); n <= uint(h.max); n++ {
		lim := h.limit[n]
		if lim == -1 {
			continue
		}
		for f.nb < n {
			if err := f.moreBits(); err != nil {
				return 0, err
			}
		}
		v := int(f.b & uint32(1<<n-1))
		v <<= 16 - n
		v = int(reverseByte[v>>8]) | int(reverseByte[v&0xFF])<<8 // reverse bits
		if v <= lim {
			f.b >>= n
			f.nb -= n
			return h.codes[v-h.base[n]], nil
		}
	}
	return 0, CorruptInputError(f.roffset)
}

// Flush any buffered output to the underlying writer.
func (f *decompressor) flush(step func(*decompressor)) {
	f.toRead = f.hist[f.hw:f.hp]
	f.woffset += int64(f.hp - f.hw)
	f.hw = f.hp
	if f.hp == len(f.hist) {
		f.hp = 0
		f.hw = 0
		f.hfull = true
	}
	f.step = step
}

func makeReader(r io.Reader) Reader {
	if rr, ok := r.(Reader); ok {
		return rr
	}
	return bufio.NewReader(r)
}

// NewReader returns a new ReadCloser that can be used
// to read the uncompressed version of r.  It is the caller's
// responsibility to call Close on the ReadCloser when
// finished reading.
func NewReader(r io.Reader) io.ReadCloser {
	var f decompressor
	f.bits = new([maxLit + maxDist]int)
	f.codebits = new([numCodes]int)
	f.r = makeReader(r)
	f.hist = new([maxHist]byte)
	f.step = (*decompressor).nextBlock
	return &f
}

// NewReaderDict is like NewReader but initializes the reader
// with a preset dictionary.  The returned Reader behaves as if
// the uncompressed data stream started with the given dictionary,
// which has already been read.  NewReaderDict is typically used
// to read data compressed by NewWriterDict.
func NewReaderDict(r io.Reader, dict []byte) io.ReadCloser {
	var f decompressor
	f.r = makeReader(r)
	f.hist = new([maxHist]byte)
	f.bits = new([maxLit + maxDist]int)
	f.codebits = new([numCodes]int)
	f.step = (*decompressor).nextBlock
	f.setDict(dict)
	return &f
}

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package time

// Sleep pauses the current goroutine for the duration d.
func Sleep(d Duration)

func nano() int64 {
	sec, nsec := now()
	return sec*1e9 + int64(nsec)
}

// Interface to timers implemented in package runtime.
// Must be in sync with ../runtime/runtime.h:/^struct.Timer$
type runtimeTimer struct {
	i      int32
	when   int64
	period int64
	f      func(int64, interface{})
	arg    interface{}
}

func startTimer(*runtimeTimer)
func stopTimer(*runtimeTimer) bool

// The Timer type represents a single event.
// When the Timer expires, the current time will be sent on C,
// unless the Timer was created by AfterFunc.
type Timer struct {
	C <-chan Time
	r runtimeTimer
}

// Stop prevents the Timer from firing.
// It returns true if the call stops the timer, false if the timer has already
// expired or stopped.
// Stop does not close the channel, to prevent a read from the channel succeeding
// incorrectly.
func (t *Timer) Stop() (ok bool) {
	return stopTimer(&t.r)
}

// NewTimer creates a new Timer that will send
// the current time on its channel after at least duration d.
func NewTimer(d Duration) *Timer {
	c := make(chan Time, 1)
	t := &Timer{
		C: c,
		r: runtimeTimer{
			when: nano() + int64(d),
			f:    sendTime,
			arg:  c,
		},
	}
	startTimer(&t.r)
	return t
}

func sendTime(now int64, c interface{}) {
	// Non-blocking send of time on c.
	// Used in NewTimer, it cannot block anyway (buffer).
	// Used in NewTicker, dropping sends on the floor is
	// the desired behavior when the reader gets behind,
	// because the sends are periodic.
	select {
	case c.(chan Time) <- Unix(0, now):
	default:
	}
}

// After waits for the duration to elapse and then sends the current time
// on the returned channel.
// It is equivalent to NewTimer(d).C.
func After(d Duration) <-chan Time {
	return NewTimer(d).C
}

// AfterFunc waits for the duration to elapse and then calls f
// in its own goroutine. It returns a Timer that can
// be used to cancel the call using its Stop method.
func AfterFunc(d Duration, f func()) *Timer {
	t := &Timer{
		r: runtimeTimer{
			when: nano() + int64(d),
			f:    goFunc,
			arg:  f,
		},
	}
	startTimer(&t.r)
	return t
}

func goFunc(now int64, arg interface{}) {
	go arg.(func())()
}

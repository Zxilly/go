// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

package runtime

import "internal/runtime/atomic"

// A three-state futex mutex. A contended owner leaves the sleeping state set
// so unlocking wakes a waiter, including when another M acquires the lock
// before a previously woken waiter can run.
const (
	mutexUnlocked = 0
	mutexLocked   = 1
	mutexSleeping = 2

	active_spin     = 4  // referenced in proc.go for sync.Mutex implementation
	active_spin_cnt = 30 // referenced in proc.go for sync.Mutex implementation

	// Use the same encoding of m.locks as lock_spinbit.go. Mutex locks
	// occupy the upper bits; acquirem and releasem change the low bits.
	mutexMLocksDelta = 16
)

type mWaitList struct{}

func lockVerifyMSize() {}

func mutexContended(l *mutex) bool {
	return atomic.Load(key32(&l.key)) == mutexSleeping
}

func lock(l *mutex) {
	lockWithRank(l, getLockRank(l))
}

func lock2(l *mutex) {
	gp := getg()
	if gp.m.locks < 0 {
		throw("runtime·lock: lock count")
	}
	gp.m.locks += mutexMLocksDelta
	key := key32(&l.key)
	if atomic.Cas(key, mutexUnlocked, mutexLocked) {
		return
	}
	for atomic.Xchg(key, mutexSleeping) != mutexUnlocked {
		futexsleep(key, mutexSleeping, -1)
	}
}

func unlock(l *mutex) {
	unlockWithRank(l)
}

func unlock2(l *mutex) {
	key := key32(&l.key)
	v := atomic.Xchg(key, mutexUnlocked)
	if v == mutexUnlocked {
		throw("unlock of unlocked lock")
	}
	if v == mutexSleeping {
		futexwakeup(key, 1)
	}
	gp := getg()
	gp.m.locks -= mutexMLocksDelta
	if gp.m.locks < 0 {
		throw("runtime·unlock: lock count")
	}
	if gp.m.locks == 0 && gp.preempt {
		gp.stackguard0 = stackPreempt
	}
}

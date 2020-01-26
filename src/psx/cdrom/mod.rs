//! CD-ROM interface
//!
//! The PlayStation uses an external controller for decoding and correcting CD sectors. This
//! controller is similar to the CXD1199AQ whose datasheet is available online. I try to use the
//! symbolic names defined in this datasheet where it makes sense.
//!
//! This controller communicates asynchronously with a microcontroller handling actual CD-ROM drive
//! (called the "sub-CPU" in the CXD1199AQ datasheet).
//!
//! Since you can't access the sub-CPU directly from the main CPU it's pretty difficult to
//! reverse-engineer what's going on exactly without using an oscilloscope. As a result this
//! implementation is based on No$'s specs, mednafen's source code and some educated guesses.

pub mod disc;
pub mod iso9660;

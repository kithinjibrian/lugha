/* C memory (v17, milestone 105): `c_read(p, offset, len)` copies `len` bytes
 * at `p + offset` into a new `u8[]`; `c_write(p, offset, bytes)` copies an
 * array's bytes there. They trust C's pointer and the length the program
 * gives (the spec says to check it against C's own size first); only a
 * NULL pointer is caught. Nothing keeps the pointer. */

/* `c_read(p, offset, len)`. */
LughaArray *lugha_rt_c_read(const uint8_t *p, uint64_t offset, uint64_t len) {
    if (p == NULL) {
        static const char text[] = "NULL CPtr given to c_read";
        panic_with(text, sizeof text - 1);
    }
    return bytes_of(p + offset, (int64_t)len);
}

/* `c_write(p, offset, bytes)`. */
void lugha_rt_c_write(uint8_t *p, uint64_t offset, const LughaArray *bytes) {
    if (p == NULL) {
        static const char text[] = "NULL CPtr given to c_write";
        panic_with(text, sizeof text - 1);
    }
    if (bytes->len > 0) {
        memcpy(p + offset, bytes->elems, (size_t)bytes->len);
    }
}

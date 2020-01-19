#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>

#define BUF_SIZE  40960

struct buffer {
    int fd;
    char buf[BUF_SIZE];
    size_t buf_len;
    size_t buf_pos;
    char line[BUF_SIZE];
    size_t line_pos;
};

static int read_char(struct buffer *b, char *c) {
    ssize_t n;

    for (;;) {
        if (b->buf_pos < b->buf_len) {
            *c = b->buf[b->buf_pos++];
            return 0;
        }

        b->buf_pos = 0;
        b->buf_len = 0;

        n = read(b->fd, b->buf, BUF_SIZE);
        if (n < 0) {
            return -1;
        }

        if (n == 0) {
            errno = EREMOTEIO;
            return -1;
        }

        b->buf_len = n;
    }
}

static int read_line(struct buffer *b) {
    char c = 0;

    b->line_pos = 0;

    for (;;) {
        if (read_char(b, &c)) {
            return -1;
        }

        if (c == '\n') {
            b->line[b->line_pos] = '\0';
            return 0;
        }

        b->line[b->line_pos++] = c;

        if (b->line_pos == BUF_SIZE) {
            errno = ENOMEM;
            return -1;
        }
    }
}

static void compare(const char *pa, int fa,
                    const char *pb, int fb) {
    struct buffer ba = { .fd = fa };
    struct buffer bb = { .fd = fb };
    size_t line = 0;

    for (;;) {
        if (read_line(&ba)) {
            fprintf(stderr, "Couldn't read \"%s\": %s", pa, strerror(errno));
            return;
        }

        if (read_line(&bb)) {
            fprintf(stderr, "Couldn't read \"%s\": %s", pb, strerror(errno));
            return;
        }

        line++;

        if (strcmp(ba.line, bb.line)) {
            printf("Missmatch at line %lu:\n", line);
            printf("%s:\n", pa);
            printf("  %s\n", ba.line);
            printf("%s:\n", pb);
            printf("  %s\n", bb.line);
        }
    }
}

int main(int argc, char **argv) {
    int fa = -1;
    int fb = -1;
    int ret = EXIT_FAILURE;

    if (argc < 3) {
        fprintf(stderr, "Usage: %s <fifo-path> <fifo-path>\n", argv[0]);
        goto cleanup;
    }

    mkfifo(argv[1], 0600);
    mkfifo(argv[2], 0600);

    fa = open(argv[1], O_RDONLY);
    if (fa < 0) {
        fprintf(stderr, "couldn't mkfifo(\"%s\"): %s\n", argv[1],
                strerror(errno));
        goto cleanup;
    }

    fb = open(argv[2], O_RDONLY);
    if (fb < 0) {
        fprintf(stderr, "couldn't mkfifo(\"%s\"): %s\n", argv[2],
                strerror(errno));
        goto cleanup;
    }

    compare(argv[1], fa, argv[2], fb);

    ret = EXIT_SUCCESS;

cleanup:
    if (fa >= 0) {
        close(fa);
        unlink(argv[1]);
    }
    if (fb >= 0) {
        close(fb);
        unlink(argv[2]);
    }
    return ret;
}

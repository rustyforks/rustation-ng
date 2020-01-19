NAME = fifocmd

CFLAGS = -O3 -Wall -MMD -MP

OBJS = main.o

DEPS = $(OBJS:%.o=%.d)

.PHONY : all
all: $(NAME)
	ls -lh $^

$(NAME): $(OBJS)
	$(info LD $@)
	$(CC) $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(info CC $@)
	$(CC) -c $(CFLAGS) -o $@ $<

.PHONY : clean
clean:
	$(info CLEAN)
	rm -f $(OBJS) $(DEPS)

# Be verbose if V is set
$V.SILENT:

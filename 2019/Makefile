intcode-objects := intcode.import.scm intcode.so

all: intcode-machine

$(intcode-objects): intcode.scm
	csc -s $< -J

intcode-machine: intcode-machine.scm $(intcode-objects)
	csc $<

clean:
	rm $(intcode-objects) intcode-machine

.PHONY: all clean

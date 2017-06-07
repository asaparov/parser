#
# Unix/Linux makefile
# Abulhair Saparov
#

#
# List of source files
#

DATALOG_TO_LAMBDA_CPP_SRCS=datalog_to_lambda.cpp
DATALOG_TO_LAMBDA_DBG_OBJS=$(DATALOG_TO_LAMBDA_CPP_SRCS:.cpp=.debug.o)
DATALOG_TO_LAMBDA_OBJS=$(DATALOG_TO_LAMBDA_CPP_SRCS:.cpp=.release.o)

PARSER_CPP_SRCS=parser.cpp
PARSER_DBG_OBJS=$(PARSER_CPP_SRCS:.cpp=.debug.o)
PARSER_OBJS=$(PARSER_CPP_SRCS:.cpp=.release.o)


#
# Compile and link options
#

LIBRARY_PKG_LIBS=
PKG_LIBS=-Wl,--no-as-needed -lpthread
GLIBC := $(word 2,$(shell getconf GNU_LIBC_VERSION))
GLIBC_HAS_RT := $(shell expr $(GLIBC) \>= 2.17)
ifeq "$(GLIBC_HAS_RT)" "0"
	LIBRARY_PKG_LIBS += -lrt
	PKG_LIBS += -lrt
endif

CPP=g++
WARNING_FLAGS=-Wall -Wpedantic
override CPPFLAGS_DBG += $(WARNING_FLAGS) -I. -g -march=native -std=c++11 $(PKG_LIBS)
override CPPFLAGS += $(WARNING_FLAGS) -I. -O3 -fomit-frame-pointer -DNDEBUG -march=native -std=c++11 -fno-stack-protector $(PKG_LIBS)
override LDFLAGS_DBG += -g $(LIB_PATHS)
override LDFLAGS += $(LIB_PATHS) -fwhole-program


#
# Compile command
#

-include $(DATALOG_TO_LAMBDA_OBJS:.release.o=.release.d)
-include $(DATALOG_TO_LAMBDA_DBG_OBJS:.debug.o=.debug.d)
-include $(PARSER_OBJS:.release.o=.release.d)
-include $(PARSER_DBG_OBJS:.debug.o=.debug.d)

define make_dependencies
	$(1) $(2) -c $(3).$(4) -o $(3).$(5).o
	$(1) -MM $(2) $(3).$(4) > $(3).$(5).d
	@mv -f $(3).$(5).d $(3).$(5).d.tmp
	@sed -e 's|.*:|$(3).$(5).o:|' < $(3).$(5).d.tmp > $(3).$(5).d
	@sed -e 's/.*://' -e 's/\\$$//' < $(3).$(5).d.tmp | fmt -1 | \
		sed -e 's/^ *//' -e 's/$$/:/' >> $(3).$(5).d
	@rm -f $(3).$(5).d.tmp
endef

%.release.o: %.cpp
	$(call make_dependencies,$(CPP),$(CPPFLAGS),$*,cpp,release)
%.release.pic.o: %.cpp
	$(call make_dependencies,$(CPP),$(CPPFLAGS),$*,cpp,release.pic)
%.debug.o: %.cpp
	$(call make_dependencies,$(CPP),$(CPPFLAGS_DBG),$*,cpp,debug)
%.debug.pic.o: %.cpp
	$(call make_dependencies,$(CPP),$(CPPFLAGS_DBG),$*,cpp,debug.pic)


#
# GNU Make: targets that don't build files
#

.PHONY: all debug clean distclean

#
# Make targets
#

all: datalog_to_lambda parser

debug: datalog_to_lambda_dbg parser_dbg

datalog_to_lambda: $(LIBS) $(DATALOG_TO_LAMBDA_OBJS)
		$(CPP) -o datalog_to_lambda $(CPPFLAGS) $(LDFLAGS) $(DATALOG_TO_LAMBDA_OBJS)

datalog_to_lambda_dbg: $(LIBS) $(DATALOG_TO_LAMBDA_DBG_OBJS)
		$(CPP) -o datalog_to_lambda_dbg $(CPPFLAGS_DBG) $(LDFLAGS_DBG) $(DATALOG_TO_LAMBDA_DBG_OBJS)

parser: $(LIBS) $(PARSER_OBJS)
		$(CPP) -o parser $(CPPFLAGS) $(LDFLAGS) $(PARSER_OBJS)

parser_dbg: $(LIBS) $(PARSER_DBG_OBJS)
		$(CPP) -o parser_dbg $(CPPFLAGS_DBG) $(LDFLAGS_DBG) $(PARSER_DBG_OBJS)

clean:
	    ${RM} -f *.o */*.o */*/*.o *.d */*.d */*/*.d datalog_to_lambda datalog_to_lambda.exe datalog_to_lambda_dbg datalog_to_lambda_dbg.exe parser parser_dbg parser.exe parser_dbg.exe $(LIBS)

distclean:  clean
	    ${RM} -f *~

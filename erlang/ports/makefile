## TOP LEVEL

make: test

## C

long_str = "_1_2_3_4_5_6_7_8_9_10_11_12_13_14_15_16_17_18_19_20_21_22_23_24_25_26_27_28_29_30_31_32_33_34_35_36_37_38_39_40_41_42_43_44_45_46_47_48_49_50_51_52_53_54_55_56_57_58_59_60_61_62_63_64_65_66_67_68_69_70_71_72_73_74_75_76_77_78_79_80_81_82_83_84_85_86_87_88_89_90_91_92_93_94_95_96_97_98_99_100"

.PHONY: c_compile c_test

c_compile:
	gcc string_reverser.c -o string_reverser.o
	cp string_reverser.o *_SUITE_data/

c_clean:
	rm string_reverser.o

c_test:
	echo  "alutka" | ./string_reverser.o

c_test2:
	echo  $(long_str) | ./string_reverser.o

## Erlang

logdir = ct_log

PHONY: compile test 

compile: c_compile
	erlc *.erl

clean: c_clean
	rm *.beam

test: compile
	mkdir -p $(logdir)
	ct_run -logdir $(logdir)

ifeq ($(OS),Windows_NT)
    include windows.mk
else 
    include linux.mk
endif



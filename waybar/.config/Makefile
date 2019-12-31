ibus-engine: ibus-engine.cpp
	$(CXX) ibus-engine.cpp -O3 -o ibus-engine `pkg-config --cflags --libs ibus-1.0`

run: ibus-engine
	./ibus-engine

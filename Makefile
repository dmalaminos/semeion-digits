all: practica

practica: cycles.cpp edges.cpp
	g++ cycles.cpp -o cycles
	g++ edges.cpp -o edges

clean:
	rm -rf cycles edges 

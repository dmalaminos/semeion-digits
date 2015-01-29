/**
 *  Este programa devuelve el número de "picos" de una matriz booleana que
 *  representa un dígito manuscrito (1 = trazo del carácter).
 *
 *  Buscamos en la matriz una serie de patrones que representan los bordes o
 *  picos del dígito. Utilizamos una estructura Union-Find para agrupar los
 *  bordes más cercanos (distancia euclidiana) según un valor de threshold.
 *
 *  Entrada: secuencia de 0s y 1s que representa la matriz binaria de un dígito 
 */

#include <iostream>
#include <vector>
#include <set>
#include <cmath>
using namespace std;

#define MSIZE 18
#define PSIZE 3
#define THRSH 5

typedef vector<vector<int> > Matrix;
typedef int Pattern[PSIZE][PSIZE];

struct UnionFind {
    int nSets;
    vector<int> setId;
    vector<int> setSize;

    inline UnionFind(int n) {
        nSets = n;
        setId = vector<int>(n);
        setSize = vector<int>(n);
        for (int i = 0; i < n; ++i) {
            setId[i] = i;
            setSize[i] = 1;
        }
    }

    inline int findSet(int x) {
        if (setId[x] != x) setId[x] = findSet(setId[x]);
        return setId[x];
    }

    inline void unionSet(int x, int y) {
        int rootX = findSet(x);
        int rootY = findSet(y);
        if (rootX == rootY) return;
        
        if (setSize[rootX] < setSize[rootY]) {
            setId[rootX] = rootY;
            setSize[rootY] += setSize[rootX];
            setSize[rootX] = 0;
        } else {
            setId[rootY] = rootX;
            setSize[rootX] += setSize[rootY];
            setSize[rootY] = 0;
        }

        --nSets;
    }

    inline int numSets() {
        return nSets;
    }
};

struct Pair {
    int fst, snd;

    inline Pair() {}

    inline Pair(int a, int b) {
        fst = a; snd = b;
    }
};

struct Comparator {
    bool operator() (const Pair& a, const Pair& b) const {
        return a.fst != b.fst and a.snd != b.snd;
    }
};

bool closePoint(const Pair& a, const Pair& b) {
    int dist = sqrt((a.fst - b.fst)*(a.fst - b.fst) + (a.snd - b.snd)*(a.snd - b.snd));
    return dist < THRSH;
}

bool isEdge(Matrix& m, int ci, int cj, Pattern& p, set<Pair, Comparator>& v) {
    int i = ci-1;
    for (int x = 0; x < PSIZE; ++x) {
        int j = cj-1;
        for (int y = 0; y < PSIZE; ++y) {
            if (p[x][y] != m[i][j] and p[x][y] != 2 and m[i][j] != 3) return false;
            ++j;
        }
        ++i;
    }

    //Añado al conjunto de bordes candidatos todos los puntos 
    //que coinciden con un "1" en el patrón.
    for (int x = 0; x < PSIZE; ++x) {
        for (int y = 0; y < PSIZE; ++y) {
            if (p[x][y] == 1) v.insert(Pair(ci-1+x, cj-1+y));
        }
    }

    return true;
}

int main() {
    Matrix dig(MSIZE, vector<int>(MSIZE, 0));

    for (int i = 1; i < MSIZE-1; ++i) {
        for (int j = 1; j < MSIZE-1; ++j) {
            cin >> dig[i][j];
        }
    }

    //Para todos los patrones:
    // - 0 y 1 para hacer matching con la matriz
    // - 2 como wildcard (no importa su contenido)
    // - 3 centro de un patrón encontrado (sólo en la matriz "dig")
    Pattern edgeBL = { {2, 1, 0},
                       {1, 1, 0},
                       {0, 0, 0} };

    Pattern edgeTR = { {0, 0, 0},
                       {0, 1, 1},
                       {0, 1, 2} };

    Pattern edgeTL = { {0, 0, 0},
                       {1, 1, 0},
                       {2, 1, 0} };

    Pattern edgeBR = { {0, 1, 2},
                       {0, 1, 1},
                       {0, 0, 0} };

    Pattern edgeL  = { {0, 0, 0},
                       {0, 1, 1},
                       {0, 0, 0} };    

    Pattern edgeR  = { {0, 0, 0},
                       {1, 1, 0},
                       {0, 0, 0} }; 

    Pattern edgeT  = { {0, 0, 0},
                       {0, 1, 0},
                       {0, 1, 0} }; 

    Pattern edgeB  = { {0, 1, 0},
                       {0, 1, 0},
                       {0, 0, 0} };             

    //Buscamos los patrones en la matriz
    set<Pair, Comparator> cand;
    for (int i = 1; i < MSIZE-1; ++i) {
        for (int j = 0; j < MSIZE-1; ++j) {
            if (dig[i][j] == 1) {
                if (isEdge(dig, i, j, edgeTR, cand) or isEdge(dig, i, j, edgeBL, cand) or isEdge(dig, i, j, edgeTL, cand) or isEdge(dig, i, j, edgeBR, cand) or
                    isEdge(dig, i, j, edgeT, cand) or isEdge(dig, i, j, edgeB, cand) or isEdge(dig, i, j, edgeL, cand) or isEdge(dig, i, j, edgeR, cand)) {
                    dig[i][j] = 3;
                }
            }
        }
    }

    //Agrupamos bordes cercanos
    UnionFind uf(cand.size());
    int p = 0;
    for (set<Pair, Comparator>::iterator it = cand.begin(); it != cand.end(); ++it) {
        int q = p+1;
        set<Pair, Comparator>::iterator it2 = cand.begin();
        for (int i = 0; i < p+1; ++i) ++it2;
        while (it2 != cand.end()) {
            if (closePoint(*it, *it2) and (uf.findSet(p) != uf.findSet(q))) {
                uf.unionSet(uf.findSet(p), uf.findSet(q));
            }
            ++q;
            ++it2;
        }
        ++p;
    }
    
    cout << uf.numSets() << endl;
}

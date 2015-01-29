/**
 *  Este programa devuelve el número de ciclos de una matriz booleana que
 *  representa un dígito manuscrito (1 = trazo del carácter).
 *
 *  Utilizamos un algoritmo basado en BFS con una estructura Union-Find
 *  para obtener el número de componentes conexas del grafo; tomamos que
 *  dos píxeles/vértices pertenecen a la misma CC si tienen el mismo valor
 *  en la matriz.
 *
 *  Por ejemplo: 1 tiene una CC (marco exterior), 0 tiene dos, 8 tiene tres.
 *
 *  Entrada: secuencia de 0s y 1s que representa la matriz binaria de un dígito 
 */

#include <iostream>
#include <vector>
using namespace std;

#define RSIZE 16
#define MSIZE 18

typedef vector<vector<int> > Matrix;

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

    //Devuelve el número de CCs
    inline int numSets() {
        return nSets;
    }

    //Devuelve el número de CCs que contengan sólo píxeles en 1
    inline int numWalls(Matrix& m) {
        int res = 0;
        for (int i = 0; i < setSize.size(); ++i) {
            if (setSize[i] > 0 and m[i/MSIZE][i%MSIZE] == 1) ++res;
        }
        return res;
    }
};


void checkSets(Matrix& m, UnionFind& uf, int i, int j, int x, int y) {
    if (i < 0 or j < 0 or i >= MSIZE or j >= RSIZE+2 or m[i][j] != m[x][y]) return;
    else if (uf.findSet(i*MSIZE+j) != uf.findSet(x*MSIZE+y)) {
        uf.unionSet(uf.findSet(i*MSIZE+j), uf.findSet(x*MSIZE+y));
    }
}

int main() {
    Matrix dig(MSIZE, vector<int>(MSIZE, 0));

    for (int i = 1; i < MSIZE-1; ++i) {
        for (int j = 1; j < MSIZE-1; ++j) {
            cin >> dig[i][j];
        }
    }

    UnionFind uf(MSIZE*MSIZE);
    for (int i = 0; i < MSIZE; ++i) {
        for (int j = 0; j < MSIZE; ++j) {
            checkSets(dig, uf, i-1, j, i, j);
            checkSets(dig, uf, i+1, j, i, j);
            checkSets(dig, uf, i, j-1, i, j);
            checkSets(dig, uf, i, j+1, i, j);
            checkSets(dig, uf, i-1, j-1, i, j);
            checkSets(dig, uf, i+1, j+1, i, j);
            checkSets(dig, uf, i+1, j-1, i, j);
            checkSets(dig, uf, i-1, j+1, i, j);
        }
    }
    cout << uf.numSets() - uf.numWalls(dig) << endl;
}

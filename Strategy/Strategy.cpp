#include <iostream>
#include <unistd.h>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <ctime>
#include <cmath>
#include <utility>
#include <cassert>
#include <algorithm>
#include "Point.h"
#include "Strategy.h"
#include "Judge.h"
#define MAX_NODE 1000000
#define TIME_LIMIT 3
#define ROUND_MAC   0
#define ROUND_USR   1
#define INFD 1e8
using namespace std;
struct Node {
  double score;
  long long tot;
  int nxt[12];

  void clear() {
    score = 0;
    tot = 1;
    memset(nxt, 255, sizeof(nxt));
  }
};
Node a[MAX_NODE];
int size, root;
time_t global_start;
void clear() {
  size = 1;
  root = 0;
  a[0].clear();
  global_start = time(0);
  srand(global_start);
}
double Simulation(const int M, const int N, int *top, int **board,
                  const int lastX, const int lastY, const int noX, const int noY, const int dep) {

  if (dep == ROUND_MAC && userWin(lastX, lastY, M, N, board)) return 0;
  if (dep == ROUND_USR && machineWin(lastX, lastY, M, N, board)) return 1;
  if (isTie(N, top)) return 0.5;

  int posible = 0;
  for (int i = 0; i < N; i++) if (top[i]) posible++;
  assert(posible);
  int idx = rand() % posible + 1;
  int y = 0;

  for (; idx; y++)
    if (top[y]) {
      idx--;
      if (!idx) break;
    }
  assert(top[y] && !idx);

  int x = top[y] - 1;
  assert(board[x][y] == 0);
  board[x][y] = 2 - dep;
  top[y]--;
  if (x - 1 == noX && y == noY) top[y]--;
  double score = Simulation(M, N, top, board, x, y, noX, noY, dep ^ 1);
  board[x][y] = 0;
  top[y] = x + 1;
  return score;
}
int SelectNode(int M, int N, int **board, int *top, int id, int dep) {
  int idx = -1;
  double best = -1;
  assert(a[id].tot > 0);
  double tmp;
  for (int i = 0; i < N; i++) {
    if (a[id].nxt[i] == -1 && top[i]) {
      tmp = 1 + sqrt(2 * log(a[id].tot));
      if (tmp > best) {
        best = tmp;
        idx = i;
      }
    }
    if (a[id].nxt[i] != -1 && top[i]) {
      if (a[a[id].nxt[i]].tot > 0) {
        tmp = a[a[id].nxt[i]].score / a[a[id].nxt[i]].tot + sqrt(2 * log(a[id].tot) / a[a[id].nxt[i]].tot);
      } else {
        assert(a[a[id].nxt[i]].tot == -1);
        tmp = a[a[id].nxt[i]].score;
        if (tmp == 1) tmp = INFD;
      }
      if (tmp > best) {
        best = tmp;
        idx = i;
      }
    }
  }
  assert(idx != -1);
  return idx;
}
double Select(const int M, const int N, int *top, int **board,
              const int lastX, const int lastY, const int noX, const int noY, int &id, const int dep) {

  if (id != -1 && a[id].tot < 0) {
    if (dep == ROUND_USR)
      return a[id].score;
    else
      return 1 - a[id].score;
  }

  double score;
  if (id == -1) {
    if (size < MAX_NODE) {
      id = size++;
      a[id].clear();

      if (dep == ROUND_MAC && userWin(lastX, lastY, M, N, board)) {
        a[id].tot = -1;
        a[id].score = 1;
        return 0;
      }
      if (dep == ROUND_USR && machineWin(lastX, lastY, M, N, board)) {
        a[id].tot = -1;
        a[id].score = 1;
        return 1;
      }
      if (isTie(N, top)) {
        a[id].tot = -1;
        a[id].score = 0.5;
        return 0.5;
      }
      score = Simulation(M, N, top, board, lastX, lastY, noX, noY, dep);
    } else {
      return Simulation(M, N, top, board, lastX, lastY, noX, noY, dep);
    }
  } else {
    int idx = SelectNode(M, N, board, top, id, dep);
    int x = top[idx] - 1, y = idx;
    top[idx]--;
    if (x - 1 == noX && y == noY) top[idx]--;
    assert(x >= 0 && x < M && y >= 0 && y < N);
    assert(board[x][y] == 0);
    board[x][y] = 2 - dep;
    score = Select(M, N, top, board, x, y, noX, noY, a[id].nxt[idx], dep ^ 1);
    top[idx] = x + 1;
    board[x][y] = 0;
  }
  if (dep == ROUND_USR)
    a[id].score += score;
  else
    a[id].score += 1 - score;

  a[id].tot += 1;
  return score;
}
void MC(const int M, const int N, const int *_top, const int *_board,
        const int lastX, const int lastY, const int noX, const int noY) {

  int** board = new int*[M];
  for(int i = 0; i < M; i++){
    board[i] = new int[N];
    for(int j = 0; j < N; j++){
      board[i][j] = _board[i * N + j];
    }
  }
  int *top = new int[N];
  for (int i = 0; i < N; i++)
    top[i] = _top[i];

  while (time(0) - global_start < TIME_LIMIT) {
    Select(M, N, top, board, lastX, lastY, noX, noY, root, ROUND_MAC);
  }

  clearArray(M, N, board);
  delete[] top;
}
extern "C" Point* getPoint(const int M, const int N, const int* top, const int* _board,
                           const int lastX, const int lastY, const int noX, const int noY){
  clear();
  MC(M, N, top, _board, lastX, lastY, noX, noY);
  double best = -1;
  int idx = -1;
  for (int i = 0; i < N; i++)
    if (a[0].nxt[i] != -1 && top[i]) {

      double tmp = 0;
      if (a[a[0].nxt[i]].tot == -1) {
        tmp = a[a[0].nxt[i]].score;
      } else {
        tmp = a[a[0].nxt[i]].score / a[a[0].nxt[i]].tot;
      }
      //      printf("%d %.4lf\n", i, tmp);
      if (tmp > best) {
        best = tmp;
        idx = i;
      }
    }
  assert(idx != -1 && top[idx]);
  return new Point(top[idx] - 1, idx);
}
//external clear
extern "C" void clearPoint(Point* p){
  delete p;
  return;
}
void clearArray(int M, int N, int** board){
  for(int i = 0; i < M; i++){
    delete[] board[i];
  }
  delete[] board;
}

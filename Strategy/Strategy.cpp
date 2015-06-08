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
#define TIME_LIMIT 4
#define ROUND_MAC   0
#define ROUND_USR   1
#define THREAD_NUM 4
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

int table1[12];
int table2[12];

int evaluate(int x, int y, int M, int N, int **board) {
  int cnt = 1;
  for (int dx = -1; dx <= 1; dx++)
    for (int dy = -1; dy <= 1; dy++)
      if (dx != 0 || dy != 0) {
        if (0 <= x + 2 * dx && x + 2 * dx < M && 0 <= y + 2 * dy && y + 2 * dy < N &&
            board[x + dx][y + dy] && board[x + 2 * dx][y + 2 * dy] == board[x + dx][y + dy])
            cnt += 4;
      }
  return cnt;
}

double Simulation(const int M, const int N, int *top, int **board,
                  const int lastX, const int lastY, const int noX, const int noY, const int dep) {

  if (dep == ROUND_MAC && userWin(lastX, lastY, M, N, board)) return 0;
  if (dep == ROUND_USR && machineWin(lastX, lastY, M, N, board)) return 1;
  if (isTie(N, top)) return 0.5;

  int posible = 0;
  for (int i = 0; i < N; i++)
    if (top[i]) {
      table1[posible] = evaluate(top[i] - 1, i, M, N, board) + (posible ? table1[posible - 1] : 0);
      table2[posible] = i;
      posible++;
    }
  
  assert(posible);
  assert(table1[posible - 1]);
  int idx = rand() % table1[posible - 1];
  int y = table2[lower_bound(table1, table1 + posible, idx) - table1];
//  printf("%d\n", y);
  assert(0 <= y && y < N);
  assert(top[y]);

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
  double best = -1e8;
  double tot = 2 * log(a[id].tot);
  assert(a[id].tot > 0);
  double tmp;
  for (int i = 0; i < N; i++) {
    if (a[id].nxt[i] == -1 && top[i]) {
      tmp = 1 + sqrt(tot);
      if (tmp > best) {
        best = tmp;
        idx = i;
      }
    }
    if (a[id].nxt[i] != -1 && top[i]) {
      if (a[a[id].nxt[i]].tot > 0) {
        tmp = a[a[id].nxt[i]].score / a[a[id].nxt[i]].tot + sqrt(tot / a[a[id].nxt[i]].tot);
      } else {
        assert(a[a[id].nxt[i]].tot == -1);
        tmp = a[a[id].nxt[i]].score;
        if (tmp == 1) tmp = INFD;
        if (tmp == 0) tmp = -1;
      }
      if (tmp > best) {
        best = tmp;
        idx = i;
      }
    }
  }
  assert(idx != -1);
  if (tmp == INFD) return -1;
  if (tmp == -1) return -2;
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
    if (idx == -1) {
      a[id].score = 0;
      a[id].tot = -1;
      return (dep == ROUND_MAC) ? 1 : 0;
    } else if (idx == -2) {
      a[id].score = 1;
      a[id].tot = -1;
      return (dep == ROUND_MAC) ? 0 : 1;
    } else {
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
  }
  if (dep == ROUND_USR)
    a[id].score += score;
  else
    a[id].score += 1 - score;

  a[id].tot += 1;
  return score;
}

int g_M, g_N;
const int *g_top;
const int *g_board;
int g_lastX, g_lastY, g_noX, g_noY;

void MC() {

  int** board = new int*[g_M];
  for(int i = 0; i < g_M; i++){
    board[i] = new int[g_N];
    for(int j = 0; j < g_N; j++){
      board[i][j] = g_board[i * g_N + j];
    }
  }
  int *top = new int[g_N];
  for (int i = 0; i < g_N; i++)
    top[i] = g_top[i];

  while (time(0) - global_start < TIME_LIMIT) {
    Select(g_M, g_N, top, board, g_lastX, g_lastY, g_noX, g_noY, root, ROUND_MAC);
  }
  
  clearArray(g_M, g_N, board);
  delete[] top;
}
extern "C" Point* getPoint(const int M, const int N, const int* top, const int* _board,
                           const int lastX, const int lastY, const int noX, const int noY){
  clear();
  g_M = M;
  g_N = N;
  g_board = _board;
  g_top = top;
  g_lastX = lastX;
  g_lastY = lastY;
  g_noX = noX;
  g_noY = noY;
  
  MC();
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
//  printf("%d\n", size);
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

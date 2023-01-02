#№6
luna = function(r1, v1, r2, v2, t){
  x = c()
  y = c()
  for(i in (1:length(t))){
    xL = (r1 * cos(v1*t[i])) + (r2 * cos(v2*t[i]))
    x = c(x, xL)
    yL = (r1 * sin(v1*t[i])) + (r2 * sin(v2*t[i]))
    y = c(y, yL)
  }
  m_cols = cbind(x,y)
  plot(m_cols)
}

luna(90, 40, 15, 18, c(1:365))

#№ 18
x0 = function(x){
  y = matrix(x, 3, 3)
  ans = c()
  d = if (y[1,1] == y[2,2] && y[2,2] == y[3,3]) {y[1,1]} else{'n'}
  d2 = if (y[1,3] == y[2,2] && y[2,2] == y[3,1]) {y[1,3]} else{'n'}
  ans = c(ans, c(d,d2))
  for (i in c(1:3)) {
    s = if (y[i,1] == y[i,2] && y[i,2] == y[i,3]) {y[i,1]}else{'n'}
    ans = c(ans, s)
  }
  for (i in c(1:3)) {
    s = if (y[1,i] == y[2,i] && y[2,i] == y[3,i]) {y[1,i]}else{'n'}
    ans = c(ans, s)
  }
  nx = 0
  n0 = 0
  for (i in (1:length(ans))){
    if(ans[i] == ('x')) {nx = nx + 1}
    else if(ans[i] == 0) {n0 = n0 +1}
  }
  if(nx > n0){print('x')}else if(n0 > nx){print(0)}else{print(NA)}
}

x0(x = c(0,'x',0,'x','x','x','x',0,0))

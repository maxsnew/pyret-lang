fun summorial(n):
  if n == 0:
    0
  else:
    n + summorial(n + -1)
  end
end

summorial(1000)

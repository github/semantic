with cls._lock:
    found = False
with patch.object(sys, 'stdout', stdout), \
     patch.object(sys, 'stderr', stderr):
    x

// 1 · spawn + join: one tab of ../index.html. The output shown is what `lughac run` prints.
EX.spawn = {
  jobs: { main: 'var(--main)', double: 'var(--other)' },
  code: [
    K('fun') + ' double(n: i64): i64 {',
    '    println(' + Q('"double is running"') + ');',
    '    ' + K('return') + ' n * 2;',
    '}',
    '',
    K('fun') + ' main() {',
    '    ' + K('let') + ' t = ' + K('spawn') + ' double(21);',
    '    println(' + Q('"main goes on"') + ');',
    '    ' + K('let') + ' result = t.join();',
    '    println(' + Q('"result: {result}"') + ');',
    '}'],
  steps: [
    { line: 5, jobs: { main: ['run', 'running'] }, note: 'The program starts. Only <b>main</b> exists, and it has the one worker.' },
    { line: 6, jobs: { main: ['run', 'running'], double: ['ready', 'not started'] }, ticket: 'not finished',
      note: '<b>spawn</b> puts <b>double</b> in the ready line. It does <em>not</em> run yet; main keeps its turn. <b>t</b> is a ticket for the future result.' },
    { line: 7, jobs: { main: ['run', 'running'], double: ['ready', 'not started'] }, ticket: 'not finished', out: 'main goes on',
      note: 'main still has the worker, so it prints.' },
    { line: 8, jobs: { main: ['wait', 'needs t\'s result'], double: ['run', 'running'] }, ticket: 'not finished',
      note: '<b>t.join()</b>: double hasn\'t run, so main <em>can\'t</em> go on. It moves to waiting, and the next job in line, double, gets the worker.' },
    { line: 1, jobs: { main: ['wait', 'needs t\'s result'], double: ['run', 'running'] }, ticket: 'not finished', out: 'double is running',
      note: 'double runs and prints.' },
    { line: 2, jobs: { main: ['ready', 'has 42'], double: ['run', 'returning 42'] }, ticket: 'finished, result = 42', fly: ['double', 'main'],
      note: 'double returns 42. The ticket is filled in, so main\'s reason to wait is gone: it moves to the ready line.' },
    { line: 9, jobs: { main: ['run', 'running'], double: ['gone', 'done'] }, ticket: 'finished, result = 42', out: 'result: 42',
      note: 'double is finished, so main gets the worker back. <b>join()</b> gives 42, and main prints it.' },
    { line: 10, jobs: { main: ['gone', 'exiting'], double: ['gone', 'done'] }, ticket: 'finished, result = 42',
      note: 'main returns, and the program exits.' }]
};

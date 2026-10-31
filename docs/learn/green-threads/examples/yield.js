// 3 · yield_now: one tab of ../index.html. The output shown is what `lughac run` prints.
EX.yield = {
  jobs: { main: 'var(--main)', helper: 'var(--other)' },
  code: [
    K('import') + ' std.task;',
    '',
    K('fun') + ' helper() {',
    '    println(' + Q('"helper: hi"') + ');',
    '}',
    '',
    K('fun') + ' main() {',
    '    ' + K('spawn') + ' helper();',
    '    println(' + Q('"main: before yield"') + ');',
    '    task.yield_now();',
    '    println(' + Q('"main: after yield"') + ');',
    '}'],
  steps: [
    { line: 6, jobs: { main: ['run', 'running'] }, note: 'The program starts. Only <b>main</b> exists.' },
    { line: 7, jobs: { main: ['run', 'running'], helper: ['ready', 'not started'] },
      note: '<b>spawn</b> puts helper in the ready line. main keeps the worker.' },
    { line: 8, jobs: { main: ['run', 'running'], helper: ['ready', 'not started'] }, out: 'main: before yield',
      note: 'main prints.' },
    { line: 9, jobs: { helper: ['run', 'running'], main: ['ready', 'yielded'] },
      note: '<b>yield_now()</b>: main isn\'t stuck on anything, it just steps aside. So it goes to the <em>back of the ready line</em>, not to waiting. helper, at the front, gets the worker.' },
    { line: 3, jobs: { helper: ['run', 'running'], main: ['ready', 'yielded'] }, out: 'helper: hi',
      note: 'helper prints.' },
    { line: 10, jobs: { main: ['run', 'running'], helper: ['gone', 'done'] }, out: 'main: after yield',
      note: 'helper finishes. main is next in line, gets the worker back and prints.' },
    { line: 11, jobs: { main: ['gone', 'exiting'], helper: ['gone', 'done'] },
      note: 'main returns, and the program exits. Without the yield, helper would never have run: main would exit first, and leftover green threads are dropped.' }]
};

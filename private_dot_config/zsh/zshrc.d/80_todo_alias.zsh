TODO_DIRS=()
TODO_DIRS+=(~/Dropbox/todo)

for todo_dir in $TODO_DIRS
do
    alias todo='$EDITOR $todo_dir/{todo,done}.txt'
    break
done


for i in $(seq $(read nb; echo $nb)); do
  printf 'Case #%d: ' $i
  read dim; read line;
  echo $line | tr SE ES
done

# SiPoSo

## Setup Process

### Assuming a observer exists for the setup process:

1. Open erlang node `erl -name observer@hostname -setcookie supermegapolycookie`.

2. Start the **manager** in a node at the manager machine e.g `erl -name manager@20.126.76.228 -setcookie supermegapolycookie -kernel inet_dist_listen_min 41161 inet_dist_listen_max 41161`. In this case port **4639** for epmd and **41161** chosen **must be open**.

3. Start the **supervisor** in a node at the supervisor machine e.g `erl -name supervisor@20.104.85.133 -setcookie supermegapolycookie -kernel inet_dist_listen_min 41161 inet_dist_listen_max 41161`. Ports same as point 2.

4. Use `net_adm:ping('manager@20.126.76.228')` and `net_adm:ping('supervisor@20.104.85.133')` from the observer. Load the polymanager to manager and polyworker, polymath and polysupervisor to supervisor from the observer with nl(module_name). This makes so only the .beams will be present temporarly in the other machines.

5. On the **supervisor** do: `polysupervisor:start_link(lenient)`.

6. On the **manager** do: `polymanager:start()`.

7. Start a **client** e.g `erl -name client@hostname -setcookie supermegapolycookie`

8. Use `client:list()` to get **available servers**.

9. Query for **sum**, **sub**, **mult** with `client:operation(Poly1,Poly2,SleepTime,Server)`, **fact**, **is_prime**, **fib** with `client:operation(Number,SleepTime,Server)` and **maclaurionSeries** with `client:maclaurionSeries(End,Expoent,SleepTime,Server)`.
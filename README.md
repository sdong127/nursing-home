# COVID-19 Agent-Based Nursing Home Model

 <font size="4"> This code implements an agent-based model of COVID-19 spread in nursing homes.
  
  <img src="https://github.com/abilinski/BackToSchool2/blob/master/4%20-%20Output/Paper%201/Saved%20figures/Fig1.png" width="800" class="center"/>

The model includes students (organized into households with siblings and parents).  Each elementary student is assigned to a classroom with a primary teacher.  There are other adults in the school, some of who circulate between classrooms (e.g. music, art, and special education) and some of whom do not (e.g. administrators, counselors, and pull-out special education).  In addition to in-classroom interactions, the user can specify The base model is parameterized to the population distribution of Maryland, but this can be modified by adjusting the input file passed to the synthpop variable in the function make_class().  Users pass parameters to the function mult_runs(), which will call the model multiple times, allowing full representation of model stochasticity.
 
 Options for interventions include:
 1. Reducing the classroom attack rate through masking, distancing, and other mitigation measures
 2. Reducing class size
 3. Isolation and quarantine
 4. Alternative schedules
 5. Teacher vaccination
 6. Screening

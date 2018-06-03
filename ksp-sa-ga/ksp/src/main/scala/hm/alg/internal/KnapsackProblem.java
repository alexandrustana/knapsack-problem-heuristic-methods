package hm.alg.internal;

/**
 * @author Alexandru Stana, alexandru.stana@busymachines.com
 * @since 02/06/2018
 */

import java.io.Console;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class KnapsackProblem {

    private boolean      verbose                     = false;
    private boolean      mutation                    = false;
    private int          crossover_count             = 0;
    private int          clone_count                 = 0;
    private int          number_of_items             = 0;
    private int          population_size             = 0;
    private int          maximum_generations         = 30;
    private int          generation_counter          = 1;
    private double       knapsack_capacity           = 0;
    private double       prob_crossover              = 0.5;
    private double       prob_mutation               = 0.03;
    private double       total_fitness_of_generation = 0;
    private List<Double> value_of_items              = new ArrayList<Double>();
    private List<Double> weight_of_items             = new ArrayList<Double>();
    private List<Double> fitness                     = new ArrayList<>();
    private List<Double> best_fitness_of_generation  = new ArrayList<Double>();
    private List<Double> mean_fitness_of_generation  = new ArrayList<Double>();
    private List<String> population                  = new ArrayList<String>();
    private List<String> breed_population            = new ArrayList<String>();
    private List<String> best_solution_of_generation = new ArrayList<String>();


    public KnapsackProblem(int number_of_items, double knapsack_capacity, List<Double> value_of_items, List<Double> weight_of_items, int population_size, int maximum_generations, double prob_crossover, double prob_mutation, double prob_cloning) {

        this.number_of_items = number_of_items;
        this.knapsack_capacity = knapsack_capacity;
        this.value_of_items = value_of_items;
        this.weight_of_items = weight_of_items;
        this.population_size = population_size;
        this.maximum_generations = maximum_generations;
        this.prob_mutation = prob_mutation;
//        this.prob_cloning = prob_cloning;

        this.buildKnapsackProblem();

    }


    public void buildKnapsackProblem() {

        this.makePopulation();

        this.evalPopulation();

        this.best_solution_of_generation.add(this.population.get(this.getBestSolution()));

        this.mean_fitness_of_generation.add(this.getMeanFitness());

        this.best_fitness_of_generation.add(this.evalGene(this.population.get(this.getBestSolution())));

        if (this.maximum_generations > 1) {
            makeFurtherGenerations();
        }

    }


    private void makeFurtherGenerations() {

        for (int i = 1; i < this.maximum_generations; i++) {

            if ((this.maximum_generations > 4) && (i > 4)) {

                double a = this.mean_fitness_of_generation.get(i - 1);
                double b = this.mean_fitness_of_generation.get(i - 2);
                double c = this.mean_fitness_of_generation.get(i - 3);

                if (a == b && b == c) {
                    maximum_generations = i;
                    break;
                }
            }

            this.crossover_count = 0;
            this.clone_count = 0;
            this.mutation = false;

            for (int j = 0; j < this.population_size / 2; j++) {
                this.breedPopulation();
            }

            this.fitness.clear();

            this.evalBreedPopulation();

            for (int k = 0; k < this.population_size; k++) {
                this.population.set(k, this.breed_population.get(k));
            }

            this.breed_population.clear();

            this.best_solution_of_generation.add(this.population.get(this.getBestSolution()));

            this.mean_fitness_of_generation.add(this.getMeanFitness());

            this.best_fitness_of_generation.add(this.evalGene(this.population.get(this.getBestSolution())));
        }
    }

    private void showOptimalList() {

        double best_fitness = 0;
        int    best_gen     = 0;

        for (int z = 0; z < this.maximum_generations - 1; z++) {
            if (this.best_fitness_of_generation.get(z) > best_fitness) {
                best_fitness = this.best_fitness_of_generation.get(z);
                best_gen = z;
            }
        }
        String optimal_list = this.best_solution_of_generation.get(best_gen);
        for (int y = 0; y < this.number_of_items; y++) {
            if (optimal_list.substring(y, y + 1).equals("1")) {
                System.out.print((y) + " ");
            }
        }

    }

    public Double getResult() {

        double best_fitness = 0;
        int    best_gen     = 0;

        for (int z = 0; z < this.maximum_generations - 1; z++) {
            if (this.best_fitness_of_generation.get(z) > best_fitness) {
                best_fitness = this.best_fitness_of_generation.get(z);
                best_gen = z;
            }
        }
        List<Integer> r            = new ArrayList<>();
        String        optimal_list = this.best_solution_of_generation.get(best_gen);
        for (int y = 0; y < this.number_of_items; y++) {
            if (optimal_list.substring(y, y + 1).equals("1")) {
                r.add(y);
            }
        }
        return r.stream().map(i -> value_of_items.get(i)).reduce((x, y) -> x + y).get();
    }

    private void breedPopulation() {

        int gene_1;
        int gene_2;

        generation_counter = generation_counter + 1;

        if (population_size % 2 == 1) {
            breed_population.add(best_solution_of_generation.get(generation_counter - 1));
        }

        gene_1 = selectGene();
        gene_2 = selectGene();

        crossoverGenes(gene_1, gene_2);
    }


    private void mutateGene() {

        double rand_mutation = Math.random();
        if (rand_mutation <= prob_mutation) {

            mutation = true;
            String mut_gene;
            String new_mut_gene;
            Random generator  = new Random();
            int    mut_point  = 0;
            double which_gene = Math.random() * 100;

            if (which_gene <= 50) {
                mut_gene = breed_population.get(breed_population.size() - 1);
                mut_point = generator.nextInt(number_of_items);
                if (mut_gene.substring(mut_point, mut_point + 1).equals("1")) {
                    new_mut_gene = mut_gene.substring(0, mut_point) + "0" + mut_gene.substring(mut_point);
                    breed_population.set(breed_population.size() - 1, new_mut_gene);
                }
                if (mut_gene.substring(mut_point, mut_point + 1).equals("0")) {
                    new_mut_gene = mut_gene.substring(0, mut_point) + "1" + mut_gene.substring(mut_point);
                    breed_population.set(breed_population.size() - 1, new_mut_gene);
                }
            }
            if (which_gene > 50) {
                mut_gene = breed_population.get(breed_population.size() - 2);
                mut_point = generator.nextInt(number_of_items);
                if (mut_gene.substring(mut_point, mut_point + 1).equals("1")) {
                    new_mut_gene = mut_gene.substring(0, mut_point) + "0" + mut_gene.substring(mut_point);
                    breed_population.set(breed_population.size() - 1, new_mut_gene);
                }
                if (mut_gene.substring(mut_point, mut_point + 1).equals("0")) {
                    new_mut_gene = mut_gene.substring(0, mut_point) + "1" + mut_gene.substring(mut_point);
                    breed_population.set(breed_population.size() - 2, new_mut_gene);
                }
            }
        }
    }


    private int selectGene() {

        double rand = Math.random() * total_fitness_of_generation;

        for (int i = 0; i < population_size; i++) {
            if (rand <= fitness.get(i)) {
                return i;
            }
            rand = rand - fitness.get(i);
        }

        return 0;
    }


    private void crossoverGenes(int gene_1, int gene_2) {

        String new_gene_1;
        String new_gene_2;

        double rand_crossover = Math.random();
        if (rand_crossover <= prob_crossover) {
            crossover_count = crossover_count + 1;
            Random generator   = new Random();
            int    cross_point = generator.nextInt(number_of_items) + 1;

            new_gene_1 = population.get(gene_1).substring(0, cross_point) + population.get(gene_2).substring(cross_point);
            new_gene_2 = population.get(gene_2).substring(0, cross_point) + population.get(gene_1).substring(cross_point);

            breed_population.add(new_gene_1);
            breed_population.add(new_gene_2);
        } else {
            clone_count = clone_count + 1;
            breed_population.add(population.get(gene_1));
            breed_population.add(population.get(gene_2));
        }

        mutateGene();
    }


    private int getBestSolution() {
        int    best_position = 0;
        double this_fitness  = 0;
        double best_fitness  = 0;
        for (int i = 0; i < population_size; i++) {
            this_fitness = evalGene(population.get(i));
            if (this_fitness > best_fitness) {
                best_fitness = this_fitness;
                best_position = i;
            }
        }
        return best_position;
    }


    private double getMeanFitness() {
        double total_fitness = 0;
        double mean_fitness  = 0;
        for (int i = 0; i < population_size; i++) {
            total_fitness = total_fitness + fitness.get(i);
        }
        mean_fitness = total_fitness / population_size;
        return mean_fitness;
    }


    private void evalPopulation() {
        total_fitness_of_generation = 0;
        for (int i = 0; i < population_size; i++) {
            double temp_fitness = evalGene(population.get(i));
            fitness.add(temp_fitness);
            total_fitness_of_generation = total_fitness_of_generation + temp_fitness;
        }
    }


    private void evalBreedPopulation() {
        total_fitness_of_generation = 0;
        for (int i = 0; i < population_size; i++) {
            double temp_fitness = evalGene(breed_population.get(i));
            fitness.add(temp_fitness);
            total_fitness_of_generation = total_fitness_of_generation + temp_fitness;
        }
    }


    private double evalGene(String gene) {
        double total_weight  = 0;
        double total_value   = 0;
        double fitness_value = 0;
        double difference    = 0;
        char   c             = '0';

        for (int j = 0; j < number_of_items; j++) {
            c = gene.charAt(j);
            if (c == '1') {
                total_weight = total_weight + weight_of_items.get(j);
                total_value = total_value + value_of_items.get(j);
            }
        }
        difference = knapsack_capacity - total_weight;
        if (difference >= 0) {
            fitness_value = total_value;
        }

        return fitness_value;
    }


    private void makePopulation() {
        for (int i = 0; i < population_size; i++) {
            population.add(makeGene());
        }
    }


    private String makeGene() {

        StringBuilder gene = new StringBuilder(number_of_items);

        char c;

        for (int i = 0; i < number_of_items; i++) {
            c = '0';
            double rnd = Math.random();
            if (rnd > 0.5) {
                c = '1';
            }
            gene.append(c);
        }
        return gene.toString();
    }


    private void getInput() {

        String input;

        Console c = System.console();
        if (c == null) {
            System.err.println("No console.");
            System.exit(1);
        }

        input = c.readLine("Enter the number of items: ");
        if (isInteger(input)) {
            number_of_items = Integer.parseInt(input);
        } else {
            System.out.println("Not a number. Please try again.");
            System.exit(1);
        }

        for (int i = 0; i < number_of_items; i++) {
            input = c.readLine("Enter the length of item " + (i + 1) + ": ");
            if (isDouble(input)) {
                weight_of_items.add(Double.parseDouble(input));
            } else {
                System.out.println("Not a number. Please try again.");
                System.exit(1);
            }

            input = c.readLine("Enter the value of item " + (i + 1) + ": ");
            if (isDouble(input)) {
                value_of_items.add(Double.parseDouble(input));
            } else {
                System.out.println("Not a number. Please try again.");
                System.exit(1);
            }
        }

        input = c.readLine("Enter the knapsack capacity: ");
        if (isInteger(input)) {
            knapsack_capacity = Integer.parseInt(input);
        } else {
            System.out.println("Not a number. Please try again.");
            System.exit(1);
        }

        input = c.readLine("Enter the population size: ");
        if (isInteger(input)) {
            population_size = Integer.parseInt(input);
        } else {
            System.out.println("Not a number. Please try again.");
            System.exit(1);
        }
    }


    public static boolean isInteger(String str) {
        try {
            Integer.parseInt(str);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }


    public static boolean isDouble(String str) {
        try {
            Double.parseDouble(str);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

}


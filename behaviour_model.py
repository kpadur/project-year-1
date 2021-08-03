# -*- coding: utf-8 -*-
"""
Created on Fri Jul 16 08:59:40 2021

@author: kpadur
"""

import random
import csv
import copy
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import networkx as nx
from networkx.generators.random_graphs import erdos_renyi_graph

class BehaviourModel():
    """Describes the creation and behaviour of agents in an environment."""
# =============================================================================
# Create environment
# =============================================================================
    def __init__(self, regular_customers, service_providers, malicious_customers,
                 can_always_change, connection_probability):
        self.customers_list = []
        self.service_providers_list = []
        self.malicious_customers_list = []
        self.susceptible_customers_list = []
        self.adopted_customers_list = []
        self.customers_graph = nx.Graph()
        self.customers_graph_dict = dict
        self.number_of_service_providers = service_providers
        self.number_of_regular_customers = regular_customers
        self.number_of_malicious_customers = malicious_customers
        self.number_of_customers = self.number_of_regular_customers +\
            self.number_of_malicious_customers
        self.percentage_change_always = can_always_change
        self.connection_probability = connection_probability
        self.graph()
        self.customers()
        self.service_providers()
        self.malicious_customers()
        self.can_always_change()
        self.customer_attributes()
        self.customers_continue_with_same_provider = []

    def graph(self):
        """Create graph structure."""
        c_graph = erdos_renyi_graph(self.number_of_customers, self.connection_probability)
        self.customers_graph_dict = nx.to_dict_of_lists(c_graph)
        self.customers_graph.add_nodes_from(c_graph)
        self.customers_graph.add_edges_from(c_graph.edges)

    def customers(self):
        """Create customers."""
        for customer in self.customers_graph_dict:
            self.customers_list.append(customer)

    def service_providers(self):
        """Create service providers."""
        while len(self.service_providers_list) < self.number_of_service_providers:
            service_provider = random.randrange(len(self.customers_list),
                                len(self.customers_list)+self.number_of_service_providers, 1)
            if service_provider not in self.service_providers_list:
                self.service_providers_list.append(service_provider)

    def malicious_customers(self):
        """Create malicious agents."""
        while len(self.malicious_customers_list) < (self.number_of_malicious_customers):
            malicious_agent=random.choice(self.customers_list)
            self.malicious_customers_list.append(malicious_agent)

    def can_always_change(self):
        """choose customers, not malicious, who can change from beginning."""
        self.customers_always_change = []
        number_change_always = self.percentage_change_always*len(self.customers_list)
        while len(self.customers_always_change) < (number_change_always):
            change_agent=random.choice(self.customers_list)
            if change_agent not in self.malicious_customers_list:
                self.customers_always_change.append(change_agent)

    def customer_attributes(self):
        """Create attributes for customers."""
        for customer in self.customers_graph.nodes:
            self.customers_graph.nodes[customer]["customer"]=customer
            self.customers_graph.nodes[customer]["states"]=[]
            if customer in self.malicious_customers_list:
                state=(customer,"adopted",-1)
                self.adopted_customers_list.append(customer)
                self.customers_graph.nodes[customer]["enter_model"]=0
            else:
                state=(customer,"susceptible",-1) # -1 indicates before run
                self.susceptible_customers_list.append(customer)
                self.customers_graph.nodes[customer]["enter_model"]=random.choice(range(0,20))
            self.customers_graph.nodes[customer]["states"].append(state)
            if customer not in self.customers_always_change:
                self.customers_graph.nodes[customer]["change"]="cannot_change"
            else:
                self.customers_graph.nodes[customer]["change"]="can_change"
            self.customers_graph.nodes[customer]["neighbours"]=self.customers_graph_dict[customer]
            self.customers_graph.nodes[customer]["service_provider"]=\
                random.choice(self.service_providers_list)
            self.customers_graph.nodes[customer]["service_provider_init"]=\
                self.customers_graph.nodes[customer]["service_provider"]
            self.customers_graph.nodes[customer]["threshold"]=random.randint(1,10000)/10000
            self.customers_graph.nodes[customer]["experiences_list"]=[]
            self.customers_graph.nodes[customer]["direct_trust_list"]=[]
            self.customers_graph.nodes[customer]["others_trust_list"]=[]
            self.customers_graph.nodes[customer]["trust_list"]=[]

    def colour_graph(self):
        """Colour graph."""
        colours_of_nodes = []
        for customer in self.customers_graph:
            if customer in self.malicious_customers_list:
                colours_of_nodes.append('orange')
            else:
                if customer in self.adopted_customers_list:
                    colours_of_nodes.append('red')
                else:
                    colours_of_nodes.append('blue')
        nx.draw(self.customers_graph, node_color=colours_of_nodes, with_labels=True)
        plt.show(self.customers_graph)

# =============================================================================
# Show behaviour of agents
# =============================================================================
    def generate_state(self, customer, time_step):
        """Generates state at each timestep."""
        if customer in self.malicious_customers_list:
            state = (customer, "adopted", time_step)
        elif customer not in self.adopted_customers_list:
            state = (customer, "susceptible", time_step)
        else:
            state = (customer, "susceptible", time_step)
            self.adopted_customers_list.remove(customer)
            self.susceptible_customers_list.append(customer)
        self.customers_graph.nodes[customer]["states"].append(state)

    def behaviour_with_change(self, customer, unavailable_provider, time_step):
        """Execute everyday behaviour of agents with option to change."""
        self.receive_service(customer, unavailable_provider, time_step)
        self.evaluate_direct_trust(customer, time_step)
        self.receive_info_from_neighbours(customer, time_step)
        self.evaluate_trust(customer, time_step)
        self.consider_changing_provider(customer, time_step)

    def behaviour_without_change(self, customer, unavailable_provider, time_step):
        """Execute everyday behaviour of agents without option to change."""
        self.receive_service(customer, unavailable_provider, time_step)
        self.evaluate_direct_trust(customer, time_step)
        self.receive_info_from_neighbours(customer, time_step)
        self.evaluate_trust(customer, time_step)

    def receive_service(self, customer, unavailable_provider, time_step):
        """Describes how customer receives and records experience."""
        service_provider = self.customers_graph.nodes[customer]["service_provider"]
        if time_step <=2 or service_provider != unavailable_provider:
            level_of_service = random.randint(0,1)
        else:
            level_of_service = 0
        experience = (customer, service_provider, level_of_service, time_step)
        self.customers_graph.nodes[customer]["experiences_list"].append(experience)

    def evaluate_direct_trust(self, customer, time_step):
        """Describes how customer evaluated direct trust."""
        service_provider = self.customers_graph.nodes[customer]["service_provider"]
        experience_values = [l[2] for l in self.customers_graph.nodes[customer]["experiences_list"]\
                             if l[1]==service_provider]
        lambda_d = sum(experience_values)/len(experience_values)
        direct_trust = (customer, service_provider, lambda_d, time_step)
        self.customers_graph.nodes[customer]["direct_trust_list"].append(direct_trust)

    def receive_info_from_neighbours(self, customer, time_step):
        """Describes how customer receives trust info from neighbours."""
        if len(self.customers_graph_dict[customer])>=1 and time_step!=0:
            for neighbour in self.customers_graph_dict[customer]:
                if time_step > self.customers_graph.nodes[neighbour]["enter_model"]:
                    service_provider = self.customers_graph.nodes[neighbour]["service_provider"]
                    lambda_d_b = [d[2] for d in \
                                 self.customers_graph.nodes[neighbour]["direct_trust_list"]\
                                if d[0]==neighbour and d[3] == (time_step-1)]
                    lambda_d_b = lambda_d_b[0]
                    neighbours_trust = (neighbour, service_provider, lambda_d_b, time_step)
                    self.customers_graph.nodes[customer]["others_trust_list"]\
                        .append(neighbours_trust)

    def evaluate_trust(self, customer, time_step):
        """Describes how customer evaluated trust in service provider."""
        if time_step == 0 or len(self.customers_graph.nodes[customer]["neighbours"])<1 or\
                len([y[1] for y in self.customers_graph.nodes[customer]["others_trust_list"]
                     if y[1]==self.customers_graph.nodes[customer]["service_provider"]
                     and y[3]==time_step])==0:
            lambda_z = [d[2] for d in self.customers_graph.nodes[customer]["direct_trust_list"]
                        if d[3]==time_step][0]
        else:
            lambda_d = [d[2] for d in self.customers_graph.nodes[customer]["direct_trust_list"]
                        if d[3]==time_step][0]
            list_lambda_y = [y[2]\
                        for y in self.customers_graph.nodes[customer]["others_trust_list"]\
                        if y[1]==self.customers_graph.nodes[customer]["service_provider"]\
                        and y[3]==time_step]
            ave_lambda_y = sum(list_lambda_y)/len(list_lambda_y)
            lambda_z = 0.8*lambda_d + 0.2*ave_lambda_y
        service_provider = self.customers_graph.nodes[customer]["service_provider"]
        trust = (customer,service_provider, lambda_z, time_step)
        self.customers_graph.nodes[customer]["trust_list"].append(trust)


    def consider_changing_provider(self, customer, time_step):
        """Describes conditions that customers consider before changing service provider."""
        trust_level = [z[2] for z in self.customers_graph.nodes[customer]["trust_list"]
                            if z[3]==time_step][0]
        distrust_level = 1-trust_level

        if distrust_level >= self.customers_graph.nodes[customer]["threshold"]:
            if len([q[1] for q in self.customers_graph.nodes[customer]["others_trust_list"]\
                    if q[1]!=self.customers_graph.nodes[customer]["service_provider"]\
                        and q[3]==time_step])==0:
                self.gamma_algorithm(customer, time_step)
            else:
                df_provider_and_trust = (pd.DataFrame([y[1:3]
                    for y in self.customers_graph.nodes[customer]["others_trust_list"]
                    if y[3]==time_step and
                    y[1]!=self.customers_graph.nodes[customer]["service_provider"]],\
                    columns = ("service_provider", "trust")).groupby("service_provider").mean())
                q_maxtrust = df_provider_and_trust["trust"].idxmax()
                self.epsilon_algorithm(customer, q_maxtrust, time_step)

    def diffusion_model(self, customer, time_step):
        """Describes how customers adopt information in social networks."""
        if customer not in self.customers_graph.nodes:
            print ("Node " + str(customer) + " is not found in nodes.")
        elif customer not in self.adopted_customers_list and len(
                self.customers_graph_dict[customer])>=1:
            adoption_level = len(list(set(self.adopted_customers_list).intersection(
                self.customers_graph_dict[customer])))/len(self.customers_graph_dict[customer])
            if adoption_level >= self.customers_graph.nodes[customer]["threshold"]:
                self.change_state(customer, time_step)
        else:
            return

    def gamma_algorithm(self, customer, time_step):
        """Describes algorithm for changing service provider if no information
        is available from neighbours"""
        probability = random.randint(1,10000)/10000
        gamma = 0.01
        if probability < gamma:
            q_new = np.random.choice(self.service_providers_list)
        else:
            q_new = self.customers_graph.nodes[customer]["service_provider"]
        if q_new == self.customers_graph.nodes[customer]["service_provider"]:
            self.customers_continue_with_same_provider.append(1)
        else:
            self.customers_continue_with_same_provider.append(0)
        self.customers_graph.nodes[customer]["service_provider"] = q_new
        self.change_state(customer, time_step)

    def epsilon_algorithm(self, customer, q_maxtrust, time_step):
        """Describes epsilon-greedy algorithm for changing service provider."""
        trust_level = [z[2] for z in self.customers_graph.nodes[customer]["trust_list"]
                            if z[3]==time_step][0]
        probability = random.randint(1,10000)/10000
        eps = 0.01
        if probability < eps or trust_level >= 1.05 * q_maxtrust:
            q_new = np.random.choice(self.service_providers_list)
        else:
            q_new = q_maxtrust
        if q_new == self.customers_graph.nodes[customer]["service_provider"]:
            self.customers_continue_with_same_provider.append(1)
        else:
            self.customers_continue_with_same_provider.append(0)
        self.customers_graph.nodes[customer]["service_provider"] = q_new
        self.change_state(customer, time_step)

    def change_state(self, customer, time_step):
        """Describes how customers change status."""
        if customer not in self.malicious_customers_list:
            self.adopted_customers_list.append(customer)
            self.susceptible_customers_list.remove(customer)
            state=(customer,"adopted",time_step)
            self.customers_graph.nodes[customer]["states"].append(state)

# =============================================================================
# Run experiment
# =============================================================================
def run_experiment(experiment):
    """Describes how experiment is run."""
    time_step = 0
    time = []
    susceptible = []
    adopted = []
    customers_have_changed = []
    changed = []
    trust_levels_all = []
    experience_levels_all=[]
    always_change = []

    iteration_number = 100
    unavailable_provider = 107 # or random.choice(experiment.service_providers_list)
    time_diffusion_added = -1 # or random.choice(range(0, (iteration_number)))

    for  iteration in range (iteration_number):
        time.append(time_step)
        for customer in experiment.customers_list:
            if time_step >= experiment.customers_graph.nodes[customer]["enter_model"]:
                experiment.generate_state(customer, time_step)
                if (iteration - experiment.customers_graph.nodes[customer]["enter_model"])<=9\
                    and experiment.customers_graph.nodes[customer]["change"] =="cannot_change":
                    experiment.behaviour_without_change(customer, unavailable_provider, time_step)
                else:
                    if customer not in experiment.malicious_customers_list and\
                        experiment.customers_graph.nodes[customer]["change"]=="cannot_change" and\
                        len([s[2] for s in experiment.customers_graph.nodes[customer]["states"]\
                                        if s[1]=="adopted"])>=1 and\
                        (time_step - [s[2]\
                                      for s in experiment.customers_graph.nodes[customer]\
                                      ["states"] if s[1]=="adopted"][-1])<=9:
                        experiment.behaviour_without_change\
                            (customer, unavailable_provider, time_step)
                    else:
                        experiment.behaviour_with_change(customer, unavailable_provider, time_step)

                times_adopted_list = [s[2] for s in experiment.customers_graph.nodes[customer]
                                      ["states"] if s[1]=="adopted"]
                if customer not in customers_have_changed and len(times_adopted_list)>=1:
                    customers_have_changed.append(customer)

                current_trust_level = [s[1:4]\
                                    for s in  experiment.customers_graph.nodes[customer]\
                                    ["trust_list"] if s[3]==time_step][0]
                trust_levels_all.append(current_trust_level)
                current_experience_level = [s[1:4]\
                                    for s in  experiment.customers_graph.nodes[customer]\
                                    ["experiences_list"] if s[3]==time_step][0]
                experience_levels_all.append(current_experience_level)
            if iteration==time_diffusion_added:
                experiment.diffusion_model(customer, time_step)
                if customer not in customers_have_changed and len(times_adopted_list)>=1:
                    customers_have_changed.append(customer)
        time_step+=1
        adopted.append(len(experiment.adopted_customers_list))
        susceptible.append(len(experiment.susceptible_customers_list))
        changed.append(len(customers_have_changed))
        always_change.append(len(experiment.customers_always_change))
        iteration+=1

# =============================================================================
# Plot results
# =============================================================================
    # First plot
    fig = plt.figure()
    subplot = fig.add_subplot()
    subplot.set_xlabel('Time')
    subplot.set_ylabel('Number of customers')
    subplot.set_ylim([0,110])
    plt.plot(time, changed, '-', color="orange", label = "Have changed")
    plt.plot(time, always_change, ':', color = "deeppink", label= "Can always change")
    plt.legend(fontsize=9)
    plt.show()

    # Second plot
    df_provider_trust_time = (pd.DataFrame(trust_levels_all,\
                        columns = ('service_provider','trust_value','time'))
                                    .groupby(['service_provider', 'time'])\
                                    ['trust_value'].mean())
    df_provider_experience_time = (pd.DataFrame(experience_levels_all,\
                        columns = ('service_provider', 'experience_value', 'time'))
                                    .groupby(['service_provider', 'time'])\
                                    ['experience_value'].mean())
    for provider in experiment.service_providers_list:
        trust_over_time = df_provider_trust_time.iloc[0:][provider]
        experience_over_time =df_provider_experience_time.iloc[0:][provider]
        timesteps = list(range(0, len(trust_over_time)))
        fig = plt.figure()
        subplot = fig.add_subplot()
        subplot.set_title('Service provider: ' + str(provider), fontsize=10)
        subplot.set_xlabel('Time')
        subplot.set_ylabel('Level')
        subplot.set_ylim([0,100])
        subplot.set_ylim([0,1])
        plt.plot(timesteps, trust_over_time, '--', color="magenta", label="Trust level")
        plt.plot(timesteps, experience_over_time, ':', color="cyan", label="Experience level")
        plt.legend(fontsize=9)
        plt.show()

    # Third plot
    fig = plt.figure()
    subplot = fig.add_subplot()
    subplot.set_xlabel('Service providers')
    subplot.set_ylabel('Number of customers')
    if unavailable_provider!='':
        subplot.text(0.99, 1.01, 'Unavailable service provider: ' + str(unavailable_provider),
            verticalalignment='bottom', horizontalalignment='right',
            transform=subplot.transAxes,
            color='salmon', fontsize=8)
    customers_service_providers = []
    popularity_data = []
    customers_service_providers_after = []
    popularity_data_after = []
    for customer in experiment.customers_list:
        customers_service_providers.append(experiment.customers_graph.nodes[customer]\
                                            ["service_provider_init"])
        customers_service_providers_after.append(experiment.customers_graph.nodes[customer]\
                                            ["service_provider"])
    for service_provider in experiment.service_providers_list:
        count = customers_service_providers.count(service_provider)
        count_after = customers_service_providers_after.count(service_provider)
        popularity = (service_provider, count)
        popularity_after = (service_provider, count_after)
        popularity_data.append(popularity)
        popularity_data_after.append(popularity_after)
    x_service_providers = [q[0] for q in popularity_data]
    y_init = [c[1] for c in popularity_data]
    z_after = [c[1] for c in popularity_data_after]
    x_axis = np.arange(len(x_service_providers))
    plt.bar(x_axis - 0.2, y_init, 0.4, color = "darkturquoise", label = 'Before model execution')
    plt.bar(x_axis + 0.2, z_after, 0.4, color = "pink", label = 'After model execution')
    subplot.set_ylim([0,60])
    plt.xticks(x_axis, x_service_providers)
    plt.legend()
    plt.show()

# =============================================================================
# Gather data from simulation
# =============================================================================
    time_diffusion_added = -1
    if unavailable_provider == '':
        unavailable_provider = 'all_available'
    else:
        unavailable_provider='one_unavailable'
    for customer in experiment.customers_list:
        customer_id = customer
        time_entered_model = experiment.customers_graph.nodes[customer]["enter_model"]
        threshold_value = experiment.customers_graph.nodes[customer]["threshold"]
        number_of_neighbours = len(experiment.customers_graph.nodes[customer]["neighbours"])
        change_group = experiment.customers_graph.nodes[customer]["change"]
        state_init = [s[1] for s in experiment.customers_graph.nodes[customer]["states"]\
                      if s[2]==-1][0]
        if customer in customers_have_changed:
            state_after = "changed"
        else:
            state_after = "not_changed"
        service_provider_init = experiment.customers_graph.nodes[customer]["service_provider_init"]
        service_provider_after = experiment.customers_graph.nodes[customer]["service_provider"]
        times_adopted = len([s[2] for s in experiment.customers_graph.nodes[customer]["states"]\
                           if s[1]=="adopted"])
        positive_experiences = [x[2] for x in experiment.customers_graph.nodes[customer]\
                                ["experiences_list"]].count(1)
        negative_experiences = [x[2] for x in experiment.customers_graph.nodes[customer]\
                                ["experiences_list"]].count(0)

        data = [customer_id, time_entered_model, threshold_value, number_of_neighbours,
                change_group,state_init, state_after, service_provider_init,
                service_provider_after,times_adopted, positive_experiences,
                negative_experiences, unavailable_provider, time_diffusion_added]
        with open('dataset.csv', 'a', encoding='UTF8', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(data)

# =============================================================================
# Execute model
# =============================================================================
model = BehaviourModel(100,4,5,0.1,0.05)
""" Executes the model. """
for execution in range(1):
    run = copy.deepcopy(model)
    test=run_experiment(run)

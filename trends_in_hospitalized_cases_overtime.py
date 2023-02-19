import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import calendar
import datetime

'''
Stores all of the data from the master csv file into a smaller
state_covid_data.csv file (For example, if you only want data from WA)
'''
def save_state_data_to_file(state : str):
    covid_data = pd.read_csv('covid_filtered.csv')
    state_covid_data = covid_data[covid_data['state'].str.contains(state)]
    state_covid_data.to_csv('{state}_covid_data.csv'.format(state=state))


'''
Combines the total count of specified patients from all the weekly recordings
into a single month and returns a dictionary that maps the total number of cases
to a date (format: month year)
'''
def congregate_month_data_patients(covid_data, type_of_patient: str) -> dict():
    # Key-Value = <Month Year, Number of Inpatient Beds>
    # Assumptions made of collection_week: MM/DD/YYYY
    hospitalized_patient_by_month = dict()

    # Go through all the weekly recorded dates and combine the 
    # data under one month and year
    for index in covid_data.index:
        time_data = covid_data['collection_week'][index].split('/')
        month = calendar.month_abbr[int(time_data[0])]
        date = month + " " + time_data[2] # Format: Month Year
        num_of_patients = covid_data[type_of_patient][index]
        if date in hospitalized_patient_by_month:
            hospitalized_patient_by_month[date] = hospitalized_patient_by_month[date] + num_of_patients
        else:
            hospitalized_patient_by_month[date] = num_of_patients

    # Sort the dictionary by date
    keys = list(hospitalized_patient_by_month.keys())
    keys.sort(key=lambda x: datetime.datetime.strptime(x,'%b %Y'))
    sorted_dict = {i: hospitalized_patient_by_month[i] for i in keys}

    return sorted_dict   

'''
1) Get the 'collection_week', 'total_adult_patients_hospitalized_confirmed_covid_7_day_avg',
and 'total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum' data from the master file (covid_filtered.csv).
2) Concatenate all the data under a single month and year instead of weekly distributions
3) Graph the results
'''
if __name__ == '__main__':
    sns.set_theme()
    covid_data = pd.read_csv('covid_filtered.csv', 
                    usecols=['collection_week', 
                            'total_adult_patients_hospitalized_confirmed_covid_7_day_avg',
                            'total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum'])
        
    monthly_infection_adult = pd.DataFrame.from_dict(congregate_month_data_patients(covid_data,
                            'total_adult_patients_hospitalized_confirmed_covid_7_day_avg'), 
                            orient='index',
                            columns=['Average Number of Adult Patients Hospitalized with Covid'])

    monthly_infection_pediatrics = pd.DataFrame.from_dict(congregate_month_data_patients(covid_data,
                            'total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum'), 
                            orient='index',
                            columns=['Average Number of Pediatric Patients Hospitalized with Covid'])
    sns.lineplot(data=monthly_infection_adult)
    sns.lineplot(data=monthly_infection_pediatrics,palette='flare')

    plt.title("Average Number of Patients Hospitalized with Covid (Jan 2021 - Feb 2023)")
    plt.xticks(rotation=45)
    plt.show()
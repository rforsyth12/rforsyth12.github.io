import csv
import string
import urllib2
import datetime as dt
import matplotlib.pyplot as plt
from matplotlib.dates import MonthLocator, DateFormatter

def get_URL(airport, startYear, startMonth, startDay, endYear, endMonth, endDay):
    return "https://www.wunderground.com/history/airport/%s/%d/%d/%d/CustomHistory.html?dayend=%d&monthend=%d&yearend=%d&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1" % (airport, startYear, startMonth, startDay, endDay, endMonth, endYear)

# See http://stackoverflow.com/questions/16283799/how-to-read-a-csv-file-from-a-url-python
def get_reader(airport):
    url = get_URL(airport, 2016, 1, 1, 2016, 12, 31)
    response = urllib2.urlopen(url)
    reader = csv.reader(response)
    reader.next() # empty list
    column_headers = reader.next()
    # key_dict maps the column headers to the appropriate index in a row
    key_dict = dict(zip(column_headers, range(0, len(column_headers) - 1)))
    return (reader, key_dict)

def get_data_by_key(airport, key):
    (reader, key_dict) = get_reader(airport)
    data = []
    for row in reader:
        data += row[key_dict[key]]
    return data

def get_all_data(airport):
    (reader, key_dict) = get_reader(airport)
    data = []
    for row in reader:
        data.append(row)
    return (data, key_dict)

def string2date(year_month_day):
    t = tuple(map(int, string.split(year_month_day, "-")))
    if len(t) == 3:
        return dt.date(*t) # Unpack the tuple
    else:
        raise Exception(t)

def dates(year_data):
    return [string2date(day_data[0]) for day_data in year_data]

def max_temp(year_data, key_dict):
    return [day_data[key_dict["Max TemperatureF"]] for day_data in year_data]

def min_temp(year_data, key_dict):
    return [day_data[key_dict["Min TemperatureF"]] for day_data in year_data]

# f1 and f2 are functions
def graph(city_data, colors, f1, f2=None, title=None, skip_days=1):
    fig, ax = plt.subplots()
    i = 0
    try:
        for (city, (year_data, key_dict)) in city_data:
            ax.plot_date(dates(year_data), f1(year_data, key_dict), "-", color=colors[i])
            if f2:
                ax.plot_date(dates(year_data), f2(year_data, key_dict), "-", color=colors[i])
            i += 1
    except:
        pass # Ignore exceptions
    ax.xaxis.set_major_locator(MonthLocator())
    ax.xaxis.set_major_formatter(DateFormatter("%b"))
    ax.xaxis.set_ticks(dates(city_data[0][1][0][0::skip_days]))
    ax.autoscale_view()
    ax.fmt_xdata = DateFormatter("%b")
    ax.grid(True)
    fig.autofmt_xdate()
    plt.title(title)
    plt.show()

def temp_graph(data_list, colors, skip_days):
    graph(data_list, colors, max_temp, min_temp, "Temperatures", skip_days)

if __name__ == "__main__":
    kiad_data = ("KIAD", get_all_data("KIAD"))
    ksfo_data = ("KSFO", get_all_data("KSFO"))
    city_data = [kiad_data, ksfo_data]
    colors = ["#0000ff", "#ffa500"]
    temp_graph(city_data, colors, 2)

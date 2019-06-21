def fill_table (mat_list, group_list, disaster_group, groups, periods, change_col = True, print_table = True):
    
    '''
    This function creates a table named table which has the groups as rows and the periods as columns.
    
    The cells contain either the number of ties between the disaster affected counties and the
    other group or the household flows depending on the matrix list you've passed. 
       
    Arguments:
       
        - mat_list: list of matrices referring to the periods in the periods list;
        - group_list: list containing the county groups (as lists);
        - disaster_group: the list containing disaster counties;
        - groups: a list with groups names;
        - periods: a list with periods names;
        - change_col: True if you want a change column added to the table, False if you don't want it.
    '''

    import pandas as pd
    
    table = pd.DataFrame(0, index=groups, columns=periods)
    
    for group in range(len(groups)):
        for period in range(len(periods)):
            table.loc[groups[group],periods[period]] = mat_list[period].loc[disaster_group,
                                                                  group_list[group]].sum(axis=1).sum(axis=0)
        
        if change_col == True:
            
            table.loc[:,'% Change'] = (table.loc[:,periods[1]] - table.loc[:,periods[0]]
                                      )/table.loc[:,periods[0]]*100

            table.loc[:,'% Change'] = table.loc[:,'% Change'].round(decimals=1)
    
    if print_table == True:
     
    	print(table)
    
    return table
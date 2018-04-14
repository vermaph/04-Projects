def summary(df):
    try:
        rs = len(df)
        cs = len(df.columns)
        feature_list = list(df.columns)
        print("The size of your dataset is: " + str(rs) + " X " +str(cs))
        print("  ")
        print("Below are your columns and the datatypes:")
        print("  ")
        for i in range(len(feature_list)):
            print(str(feature_list[i]) + ": " + str(df.dtypes[i]))
        return
    except ValueError:
            print('User has entered wrong datatype')
            return False
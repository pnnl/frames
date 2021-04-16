#ifndef series_h
#define series_h

class Series
  {
    public:
    int num;
    double *value;
    double *time;
    char *label;
    Series();
    Series(int n);
    Series(int n, char *lbl);
    ~Series();
    void SetNum(int n);
    void Add(int n, double t, double v);
    void Peak(double *year, double *val);
    double ValueAt(double year);
    double ValueAt(double year, int index);
    double Area(double x0, double x1, double y0, double y1);
    double Average(double year1, double year2);
  };

#endif
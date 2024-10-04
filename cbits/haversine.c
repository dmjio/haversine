#include "haversine.h"
#include "stdio.h"
#include "math.h"

// Function to convert degrees to radians
double deg2rad(double deg) {
    return deg * M_PI / 180.0;
}

// Function to convert degrees to radians
double rad2deg(double rad) {
  return rad * 180.0 / M_PI;
}

double haversine(double lat1, double lon1, double lat2, double lon2, double radius) {
     double dLat = deg2rad(lat2 - lat1);
     double dLon = deg2rad(lon2 - lon1);

     lat1 = deg2rad(lat1);
     lat2 = deg2rad(lat2);

     double a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1) * cos(lat2);
     double c = 2 * atan2(sqrt(a), sqrt(1 - a));

     return radius * c;
}

// Function to calculate destination point given starting point, bearing, and distance
void reverse_haversine(double lat1, double lon1, double brng, double dist, double *lat2, double *lon2, double radius) {
    double lat1_rad = deg2rad(lat1);
    double lon1_rad = deg2rad(lon1);
    double brng_rad = deg2rad(brng);

    *lat2 = asin(sin(lat1_rad) * cos(dist / radius) + cos(lat1_rad) * sin(dist / radius) * cos(brng_rad));
    *lon2 = lon1_rad + atan2(sin(brng_rad) * sin(dist / radius) * cos(lat1_rad), cos(dist / radius) - sin(lat1_rad) * sin(*lat2));

    *lat2 = rad2deg(*lat2);
    *lon2 = rad2deg(*lon2);
}

double bearing(double lat1, double lon1, double lat2, double lon2) {
    // Convert latitudes and longitudes to radians
    lat1 = deg2rad(lat1);
    lon1 = deg2rad(lon1);
    lat2 = deg2rad(lat2);
    lon2 = deg2rad(lon2);

    // Calculate bearing
    double y = sin(lon2 - lon1) * cos(lat2);
    double x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon2 - lon1);
    double bearing = rad2deg(atan2(y, x));

    // Normalize bearing to 0-360 degrees
    if (bearing < 0) {
        bearing += 360.0;
    }

    return bearing;
}

// property
// \c1 c2 -> let b,d = haversine c1 c2 r; c3 = reverse_haversine c1 b d r in (c2 == c3)

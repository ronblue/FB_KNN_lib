#include "knn.c"
#include "greatest.h"

#include "terminal_user_input.h"

//Defining tolerances for tests
#define FLOAT_TOLERANCE 0.01

TEST mode_3_inputs(void) {
  //Setup array of integers
  int inputs[3] = {3, 3, 7};

  //Pass array of integers into function
  //Check mode is correct

  ASSERT_EQ(3, mode(inputs, 3));

  PASS();
}

TEST mode_with_zero (void) {
  //Setup array of integers
  int inputs[3] = {0, 1, 1};

  //Pass array of integers into function
  //Check mode is correct

  ASSERT_EQ(1, mode(inputs, 3));

  PASS();
}

TEST mode_7_inputs(void) {
  //Setup array of integers
  int inputs[7] = {1, 2, 3, 1, 7, 8, 1};

  //Pass array of integers into function
  //Check mode is correct

  ASSERT_EQ(1, mode(inputs, 7));

  PASS();
}

//Test bimodal

//Compare two integers that are equal
TEST compare_ints(void) {
  int n1 = 1;
  int n2 = 1;
  ASSERT_EQ(compare_int(&n1, &n2), 0);
  PASS();
}

//Compare two integers that are equal
TEST compare_greater_int(void) {
  int n1 = 2;
  int n2 = 1;
  ASSERT_EQ(compare_int(&n1, &n2), 1);
  PASS();
}

//Compare two integers that are equal
TEST compare_very_different_int_negative (void) {
  int n1 = 1;
  int n2 = 4;
  ASSERT_EQ(compare_int(&n1, &n2), -1);
  PASS();
}

TEST compare_very_different_int_positive (void) {
  int n1 = 4;
  int n2 = 1;
  ASSERT_EQ(compare_int(&n1, &n2), 1);
  PASS();
}

/* A test runs various assertions, then calls PASS(), FAIL(), or SKIP(). */
TEST distance_3_dimensions(void) {
  float array1[3] = {2.0, 2.0, 2.0};
  Comparison_Point point1 = {array1, NULL};

  float array2[3] = {5.0, 5.0, 5.0};
  Point point2 = {array2, 0, NULL};

  ASSERT_IN_RANGE(5.1962, point_distance(point1, point2, 3), FLOAT_TOLERANCE);
  PASS();
}

TEST distance_10_dimensions(void) {
  float array1[10] = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
  Comparison_Point point1 = {array1, NULL};

  float array2[10] = {10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0};
  Point point2 = {array2, 0};

  ASSERT_IN_RANGE(28.4605, point_distance(point1, point2, 10), FLOAT_TOLERANCE);
  PASS();
}

TEST distance_1_dimension(void) {
  float array1[1] = {3.0};
  Comparison_Point point1 = {array1, NULL};

  float array2[1] = {6.0};
  Point point2 = {array2, 0};

  ASSERT_IN_RANGE(3.0, point_distance(point1, point2, 1), FLOAT_TOLERANCE);
  PASS();
}

TEST distance_1_dimension_fraction(void) {
  float array1[1] = {3.0};
  Comparison_Point point1 = {array1, NULL};

  float array2[1] = {3.5};
  Point point2 = {array2, 0};

  ASSERT_IN_RANGE(0.5, point_distance(point1, point2, 1), FLOAT_TOLERANCE);
  PASS();
}

//How do I initialise arrays with the {} curly braces syntax?

//Test the creation of an array (neighbours) with distances to every single point,
//taking one point and a dataset
//We need a distance associated with a point

//Test k NN search
//Ensure that the returned array contains the correct integers

//Test the nearest 1 neighbour can be found
TEST find_1_nearest_neighbour(void) {
  int k = 1;

  int category = 0;
  //Pass it the stuff it needs, the dataset,
  float dimensions[] = {5};
  Point point1 = {dimensions, category};

  //Since we've only got a length of 1, just use a pointer straight to the single point
  Point* points = &point1;
  Dataset single_point_dataset = {1, 1, points};

  float comparison_dimensions[] = {3};
  //TODO, fix the comparison point category
  Comparison_Point compare = {comparison_dimensions, NULL};

  //One point to compare to the rest
  ASSERT_EQ(category, knn_search(k, compare, &single_point_dataset));
  free(compare.neighbour);
  PASS();
}


//One dimensional, 5 point dataset, find average of k=3 neighbours
//Test the code can handle updating 3 of the 5 without having to update a distance
TEST find_3_nearest_neighbour(void) {
  //Setup
  int k = 3;

  //Pass it the stuff it needs, the dataset,
  float dimensions0[] = {5.0};
  Point point0 = {dimensions0, 0};

  float dimensions1[] = {6.0};
  Point point1 = {dimensions1, 1};

  float dimensions2[] = {7.0};
  Point point2 = {dimensions2, 1};

  float dimensions3[] = {0.0};
  Point point3 = {dimensions3, 0};

  float dimensions4[] = {-1.0};
  Point point4 = {dimensions4, 0};

  //Since we've only got a length of 1, just use a pointer straight to the single point
  Point points[5] = {point0, point1, point2, point3, point4};
  Dataset point_dataset = {1, 5, points};

  float comparison_dimensions[] = {6.5};
  //TODO, fix the comparison point category
  Comparison_Point compare = {comparison_dimensions, NULL};

  int category = knn_search(k, compare, &point_dataset);
  free(compare.neighbour);

  //One point to compare to the rest
  ASSERT_EQ(1, category);
  PASS();
}

TEST classify_int(void) {
  //The class integer to be selected
  int class = 0;

  //Using only the minimum 1 categories
  Classifier_List flower_map;
  flower_map.categories = (my_string*)malloc(sizeof(my_string));

  strcpy(flower_map.categories[0].str, "Iris");

  my_string category = classify(flower_map, class);

  ASSERT_STR_EQ("Iris", category.str);
  PASS();
  free(flower_map.categories);
}

TEST extract_field_1(void) {
  //From a string of "1.1, 1.2, 1.3, 1.4", extract field 1
  my_string test_line;
  strcpy(test_line.str, "1.1, 1.2, 1.3, 1.4");

  ASSERT_STR_EQ("1.1", extract_field(test_line, 1).str);
  PASS();
}

TEST extract_field_4(void) {
  //From a string of "1.1, 1.2, 1.3, 1.4", extract field 1
  my_string test_line;
  strcpy(test_line.str, "1.1, 1.2, 1.3, 1.4");

  ASSERT_STR_EQ("1.4", extract_field(test_line, 4).str);
  PASS();
}

TEST extract_field_different_formatting(void) {
  //From a string of "1.1, 1.2, 1.3, 1.4", extract field 1
  my_string test_line;
  strcpy(test_line.str, "1.1,,,''1.2,three, 6");

  ASSERT_STR_EQ("three", extract_field(test_line, 3).str);
  PASS();
}

TEST extract_flower_field(void) {
  //From a string of "1.1, 1.2, 1.3, 1.4", extract field 1
  my_string test_line;
  strcpy(test_line.str, "5.1,3.5,1.4,0.2,Iris-setosa");

  ASSERT_STR_EQ("Iris-setosa", extract_field(test_line, 5).str);
  PASS();
}

TEST field_2(void) {
  //From a string of "1.1, 1.2, 1.3, 1.4", extract field 1
  my_string test_line;
  strcpy(test_line.str, "5.1,3.5,1.4,0.2,Iris-setosa");

  ASSERT_STR_EQ("3.5", extract_field(test_line, 2).str);
  PASS();
}

TEST out_of_bounds(void) {
  //From a string of "1.1, 1.2, 1.3, 1.4", extract field 1
  my_string test_line;
  strcpy(test_line.str, "5.1,3.5,1.4,0.2,Iris-setosa");

  ASSERT_STR_EQ("\0", extract_field(test_line, 6).str);
  PASS();
}

TEST gets_class_int(void) {
  //Pass in a string, with a class_list which contains it, see if the correct value is returned
  my_string strings[4] = {{"mycategory1"}, {"mycategory2"}, {"mycategory3"}, {"mycategory4"}};
  Classifier_List class_list = {strings, 4};
  ASSERT_EQ(0, get_class_num(class_list.categories[0], &class_list));

  PASS();
}

TEST initialise_category(void) {
  Classifier_List new_list = new_classifier_list();
  strcpy(new_list.categories[0].str, "Testing Category");

  ASSERT_EQ(0, new_list.num_categories);
  PASS();
}

TEST create_first_category(void) {
  //Pass in a string, with a class_list which contains it, see if the correct value is returned
  my_string first_class = {"Test Category"};

  Classifier_List class_list = new_classifier_list();
  ASSERT_EQ(0, get_class_num(first_class, &class_list));
  ASSERT_STR_EQ(first_class.str, class_list.categories[0].str);

  PASS();
}

TEST create_new_category(void) {
  //Pass in a string, with a class_list which contains it, see if the correct value is returned
  my_string first_class = {"Test Category"};
  my_string second_class = {"Class2"};

  Classifier_List class_list = new_classifier_list();
  ASSERT_EQ(0, get_class_num(first_class, &class_list));
  ASSERT_STR_EQ(first_class.str, class_list.categories[0].str);

  ASSERT_EQ(1, get_class_num(second_class, &class_list));
  ASSERT_STR_EQ(second_class.str, class_list.categories[1].str);

  #ifdef DEBUG
  print_classes(class_list);
  #endif

  PASS();
}

TEST knn_accuracy(void) {
  //Comments step through the expected classification of the knn
  //for each point removed and then consider the percentage correct for that k
  //In this case k=3

  float dimensions0[] = {5.0};
  Point point0 = {dimensions0, 0};
  //Classed 1
  //Incorrect

  float dimensions1[] = {6.0};
  Point point1 = {dimensions1, 1};
  //Classed 0
  //Incorrect

  float dimensions2[] = {7.0};
  Point point2 = {dimensions2, 1};
  //Classed 0
  //Incorrect

  float dimensions3[] = {0.0};
  Point point3 = {dimensions3, 0};
  //Classed 0
  //Correct

  float dimensions4[] = {-1.0};
  Point point4 = {dimensions4, 0};
  //Classed 0
  //correct

  //Count is 2
  // 2/5=0.4

  Point points[5] = {point0, point1, point2, point3, point4};
  Dataset test_dataset = {1, 5, points};
  evaluate_knn(3, &test_dataset);

  ASSERT_IN_RANGE(0.4, evaluate_knn(3, &test_dataset), FLOAT_TOLERANCE);
  PASS();
}


//Test that the correct number is returned after a call to the string is passed to the classifier

/* Suites can group multiple tests with common setup. */
SUITE(external_suite) {
    RUN_TEST(distance_1_dimension);
    RUN_TEST(distance_1_dimension_fraction);
    RUN_TEST(distance_3_dimensions);
    RUN_TEST(distance_10_dimensions);

    RUN_TEST(classify_int);

    RUN_TEST(mode_3_inputs);
    RUN_TEST(mode_with_zero);
    RUN_TEST(mode_7_inputs);

    RUN_TEST(compare_ints);
    RUN_TEST(compare_greater_int);
    RUN_TEST(compare_very_different_int_positive);
    RUN_TEST(compare_very_different_int_negative);

    RUN_TEST(find_1_nearest_neighbour);
    RUN_TEST(find_3_nearest_neighbour);

    RUN_TEST(extract_field_1);
    RUN_TEST(extract_field_4);
    RUN_TEST(extract_field_different_formatting);
    RUN_TEST(extract_flower_field);
    RUN_TEST(field_2);
    RUN_TEST(out_of_bounds);

    //Testing the configuration manager
    RUN_TEST(gets_class_int);
    RUN_TEST(initialise_category);
    RUN_TEST(create_first_category);
    RUN_TEST(create_new_category);

    RUN_TEST(knn_accuracy);
}

/* Add definitions that need to be in the test runner's main file. */
GREATEST_SUITE(external_suite);

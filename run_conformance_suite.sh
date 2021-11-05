RUN="../python -m compiler --static somemod.py -c --output "

for filename in ./conformance_suite/*.py; do
    NAME=$(head -n 1 $filename | tail -n 1);
    EXPECT_COMPILE=$(head -n 2 $filename | tail -n 1);
    EXPECT_RUN=$(head -n 3 $filename | tail -n 1);
    if [ "$EXPECT_COMPILE" = "# This should fail." ]; then
        # Should fail
        ../python -m compiler --static "$filename" -c --output "$filename"c &> /dev/null;
        if [ $? -eq 0 ]; then
            echo "${NAME}";
            echo "should fail but passed.";
            echo "-----------"
        else
            : # do nothing
        fi
    else
        # Should pass
        ../python -m compiler --static "$filename" -c --output "$filename"c &> /dev/null;
        if [ $? -eq 0 ]; then
            if [ "$EXPECT_RUN" = "# This should error." ]; then
                ../python -m compiler --static "$filename" &> /dev/null;
                if [ $? -eq 0 ]; then
                    echo "${NAME}";
                    echo "should error but terminated.";
                    echo "-----------"
                else
                    :
                fi
            else
                if [ "$EXPECT_RUN" = "# This should terminate." ]; then
                    ../python -m compiler --static "$filename" &> /dev/null;
                    if [ $? -eq 0 ]; then
                        :
                    else
                        echo "${NAME}";
                        echo "should terminate but errored.";
                        echo "-----------"
                    fi
                else
                    : # Do nothing when there is no run spec.
                fi
            fi
        else
            echo "${NAME}";
            echo "should pass but failed.";
            echo "-----------"
        fi
    fi
    # `cat compile_and_run.txt` $filename;
done;

echo "All done."

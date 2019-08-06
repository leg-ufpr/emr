#!/bin/bash

# Remove files.
rm -rf _site/docs
rm -rf _site/slides/config_ioslides
rm -rf _site/slides/img

# Removes Rmd in slides/ and tutoriais/
find _site/slides/    -type f -name '*.Rmd' -delete
find _site/tutoriais/ -type f -name '*.Rmd' -delete

# Tree of the directories.
echo "------------------------------------------------------------------------"
echo "Files to be sent to server.\n"
tree -h -F _site/ -L 2

# Upload.
echo "------------------------------------------------------------------------"
echo "Uploading files to server.\n"
rsync -avzp \
      ./_site/ \
      --progress \
      --rsh="ssh -p$PATAXOP" \
      "$WEBLEG@$PATAXO:/home/$WEBLEG/ensino/EMR"

# Vist the homepage.
echo "------------------------------------------------------------------------"
echo "Visiting the webpage.\n"
firefox http://web.leg.ufpr.br/ensino/EMR/

exit 0

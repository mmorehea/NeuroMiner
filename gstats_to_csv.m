% Takes swcs and performs stats_tree, exports gstats to gstats.csv

%=============================| OPTIONS |=================================%
TREES_PATH      = '';      % path to your TREES toolbox directory
PRINT_PROGRESS  = true;    % print progress indications to command window
WRITE_HEADERS   = true;    % print column headers to csv
%=========================================================================%


addpath(genpath(TREES_PATH));
start_trees;
pathPrefix  = 'swcs/';
swcsPath = strcat(pathPrefix, '*.swc');

swcs = dir(swcsPath);
swcFiles = cell(length(swcs), 1);
swcNames = cell(length(swcs), 1);

% Store names, compute stats
for i = 1:length(swcs)
    swcFiles{i} = swcs(i,1).name;
    swcNames{i} = swcs(i,1).name(1:end-4);
    treePath = strcat(pathPrefix, swcFiles{i});
    load_tree(treePath);
    stats = stats_tree([],[],[],'-x');
    if PRINT_PROGRESS
        fprintf('Computed gstats for swc %d of %d...\n', i, length(swcs));
    end
end

gstatsFields = fieldnames(stats.gstats);
gstatsLastField = gstatsFields{numel(gstatsFields)};
csvFile = fopen('gstats.csv', 'w');

% Write headers to CSV file
if WRITE_HEADERS
    fprintf(csvFile, 'name,');
    for i = 1:numel(gstatsFields) - 1
        fprintf(csvFile, gstatsFields{i});
        fprintf(csvFile, ',');
    end
    fprintf(csvFile, gstatsLastField);
    fprintf(csvFile, '\r\n');
end

% Write names and stats to CSV file
for i = 1:length(swcs)
    fprintf(csvFile, swcNames{i});
    fprintf(csvFile, ',');
    for j = 1:numel(gstatsFields) - 1
       currentField = stats.gstats.(gstatsFields{j});
       fprintf(csvFile, '%d', currentField(i));
       fprintf(csvFile, ',');
    end
    fprintf(csvFile, '%d', stats.gstats.(gstatsLastField)(i));
    fprintf(csvFile, '\r\n');
    if PRINT_PROGRESS
       fprintf('Wrote gstats for swc %d of %d...\n', i, length(swcs));
    end
end

fclose(csvFile);
fprintf('All finished!\n');

% Sends results of stats_tree for each swc to gstats.csv
% NOTE: Set TREES_PATH to the path of your TREES toolbox below

TREES_PATH = '';
addpath(TREES_PATH);

start_trees;
pathPrefix  = 'swcs/';
swcsPath = strcat(pathPrefix, '*.swc');

swcs = dir(swcsPath);
swcNames = cell(length(swcs), 1);

% Store names, load stats
for i = 1:length(swcs)
    swcNames{i} = swcs(i,1).name(1:end-4);
    treePath = strcat(pathPrefix, swcFiles{i});
    load_tree(treePath);
    stats = stats_tree([],[],[],'-x');
end

gstatsFields = fieldnames(stats.gstats);
csvFile = fopen('gstats.csv', 'w');

% Write headers to CSV file
fprintf(csvFile, 'name,');
for i = 1:numel(gstatsFields) - 1
    
    fprintf(csvFile, gstatsFields{i});
    fprintf(csvFile, ',');
    
end
fprintf(csvFile, gstatsFields{numel(gstatsFields)});
fprintf(csvFile, '\r\n');

% Write names and stats to CSV file
for i = 1:length(swcs)
    
    fprintf(csvFile, swcNames{i});
    fprintf(csvFile, ',');
    
    for j = 1:numel(gstatsFields) - 1
       fprintf(csvFile, '%d', (stats.gstats.(gstatsFields{j})(i)));
       fprintf(csvFile, ',');
    end
    
    fprintf(csvFile, '%d', stats.gstats.(gstatsFields{numel(gstatsFields)})(i));
    fprintf(csvFile, '\r\n');
end

fclose(csvFile);

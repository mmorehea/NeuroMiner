% Takes swcs and performs stats_tree, exports gstats to gstats.csv

%=============================| OPTIONS |=================================%
TREES_PATH      = '/home/mdm/code/TREES';      % path to your TREES toolbox directory
PRINT_PROGRESS  = true;    % print progress indications to command window
WRITE_HEADERS   = true;    % print column headers to csv
%=========================================================================%


addpath(genpath(TREES_PATH));
start_trees;
pathPrefix  = './swcs/';
swcsPath = strcat(pathPrefix, '*.swc');

swcs = dir(swcsPath);
swcNames = cell(length(swcs), 1);


csvFile = fopen('gstats.csv', 'w');


% Store names, compute stats
for i = 1:length(swcs)
    swcName = swcs(i,1).name(1:end-4);
    treePath = strcat(pathPrefix, swcName, '.swc');
    load_tree(treePath);
    stats = stats_tree([],[],[],'-x');
    
    if PRINT_PROGRESS
        fprintf('Computed gstats for swc %d of %d...\n', i, length(swcs));
    end
    
    fprintf(csvFile, swcName);
    fprintf(csvFile, ',');
    
    gstatsFields = fieldnames(stats.gstats);
    gstatsLastField = gstatsFields{numel(gstatsFields)};
    
    for j = 1:numel(gstatsFields) - 1
       currentField = stats.gstats.(gstatsFields{j});
       fprintf(csvFile, '%d', currentField);
       fprintf(csvFile, ',');
    end
    fprintf(csvFile, '%d', stats.gstats.(gstatsLastField));
    fprintf(csvFile, '\r\n');
    stats = [];
    trees = [];
end


fclose(csvFile);
fprintf('All finished!\n');

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
swcFiles = cell(length(swcs), 1);
swcNames = cell(length(swcs), 1);


csvFile = fopen('gstats.csv', 'w');

tic;
% Store names, compute stats
for i = 1:length(swcs)
    if mod(i,100) == 0
        fprintf('Computed gstats for swc %d of %d...\n', i, length(swcs));
        toc;
        tic;
    end
        
    swcName = swcs(i,1).name(1:end-4);
    treePath = strcat(pathPrefix, swcName, '.swc');

    load_tree(treePath);
    stats = stats_tree([],[],[],'-x');
    
    
    
    sholl = sholl_tree(1, 1);
    radius = length(sholl);
    sholl = sholl_tree(1, radius/49);
    
    
    fprintf(csvFile, swcName);
    fprintf(csvFile, ',');
    
    gstatsFields = fieldnames(stats.gstats);
    gstatsLastField = gstatsFields{numel(gstatsFields)};
    
    for j = 1:numel(gstatsFields)
       currentField = stats.gstats.(gstatsFields{j});
       fprintf(csvFile, '%d', currentField);
       fprintf(csvFile, ',');
    end
    for j = 1:length(sholl)
        fprintf(csvFile, '%d', sholl(j));
        fprintf(csvFile, ',');
    end
    fprintf(csvFile, '\r\n');
    stats = [];
    trees = [];


fclose(csvFile);
fprintf('All finished!\n');

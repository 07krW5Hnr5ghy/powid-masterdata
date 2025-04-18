package com.proyect.masterdata.helpers;

import com.proyect.masterdata.repository.DepartmentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class DepartmentIdResolver {


    private final DepartmentRepository departmentRepository;

    private final Map<String, UUID> departmentCache = new HashMap<>();

    public List<UUID> resolveDepartmentIds(List<String> departmentNames) {
        if (departmentNames == null || departmentNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = departmentNames.stream()
                .map(String::toUpperCase)
                .toList();

        List<String> missingNames = upperNames.stream()
                .filter(name -> !departmentCache.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            departmentRepository.findByNameIn(missingNames)
                    .forEach(dept -> departmentCache.put(dept.getName().toUpperCase(), dept.getId()));
        }

        return upperNames.stream()
                .map(departmentCache::get)
                .filter(Objects::nonNull)
                .toList();
    }

}

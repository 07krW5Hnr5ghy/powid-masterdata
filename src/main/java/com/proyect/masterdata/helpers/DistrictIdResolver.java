package com.proyect.masterdata.helpers;

import com.proyect.masterdata.repository.DistrictRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class DistrictIdResolver {
    private final DistrictRepository districtRepository;

    private final Map<String, UUID> districtCache = new HashMap<>();

    public List<UUID> resolveDistrictIds(List<String> districtNames) {
        if (districtNames == null || districtNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = districtNames.stream()
                .map(String::toUpperCase)
                .toList();

        List<String> missingNames = upperNames.stream()
                .filter(name -> !districtCache.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            districtRepository.findByNameIn(missingNames)
                    .forEach(district ->
                            districtCache.put(district.getName().toUpperCase(), district.getId())
                    );
        }

        return upperNames.stream()
                .map(districtCache::get)
                .filter(Objects::nonNull)
                .toList();
    }

}

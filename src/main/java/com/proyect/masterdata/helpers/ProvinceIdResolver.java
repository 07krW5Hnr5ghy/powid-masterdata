package com.proyect.masterdata.helpers;

import com.proyect.masterdata.repository.ProvinceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class ProvinceIdResolver {
    private final ProvinceRepository provinceRepository;

    private final Map<String, UUID> provinceCache = new HashMap<>();

    public List<UUID> resolveProvinceIds(List<String> provinceNames) {
        if (provinceNames == null || provinceNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = provinceNames.stream()
                .map(String::toUpperCase)
                .toList();

        List<String> missingNames = upperNames.stream()
                .filter(name -> !provinceCache.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            provinceRepository.findByNameIn(missingNames)
                    .forEach(province ->
                            provinceCache.put(province.getName().toUpperCase(), province.getId())
                    );
        }

        return upperNames.stream()
                .map(provinceCache::get)
                .filter(Objects::nonNull)
                .toList();
    }
}

package com.proyect.masterdata.helpers;


import com.proyect.masterdata.repository.DeliveryPointRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class DeliveryPointIdResolver {
    private final DeliveryPointRepository deliveryPointRepository;

    private final Map<String, UUID> deliveryPointCache = new HashMap<>();

    public List<UUID> resolveDeliveryPointIds(List<String> deliveryPointNames) {
        if (deliveryPointNames == null || deliveryPointNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = deliveryPointNames.stream()
                .map(String::toUpperCase)
                .toList();

        List<String> missingNames = upperNames.stream()
                .filter(name -> !deliveryPointCache.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            deliveryPointRepository.findByNameIn(missingNames)
                    .forEach(point ->
                            deliveryPointCache.put(point.getName().toUpperCase(), point.getId())
                    );
        }

        return upperNames.stream()
                .map(deliveryPointCache::get)
                .filter(Objects::nonNull)
                .toList();
    }
}

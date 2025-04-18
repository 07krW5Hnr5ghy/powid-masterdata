package com.proyect.masterdata.helpers;


import com.proyect.masterdata.repository.CourierRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class CourierIdResolver {

    private final CourierRepository courierRepository;

    private final Map<UUID, Map<String, UUID>> courierCache = new HashMap<>();

    public List<UUID> resolveCourierIds(UUID clientId, List<String> courierNames) {
        if (clientId == null || courierNames == null || courierNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = courierNames.stream()
                .map(String::toUpperCase)
                .toList();

        courierCache.putIfAbsent(clientId, new HashMap<>());
        Map<String, UUID> clientCouriers = courierCache.get(clientId);

        List<String> missingNames = upperNames.stream()
                .filter(name -> !clientCouriers.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            courierRepository.findByClientIdAndNameIn(clientId, missingNames)
                    .forEach(courier -> clientCouriers.put(
                            courier.getName().toUpperCase(), courier.getId()
                    ));
        }

        return upperNames.stream()
                .map(clientCouriers::get)
                .filter(Objects::nonNull)
                .toList();
    }
}

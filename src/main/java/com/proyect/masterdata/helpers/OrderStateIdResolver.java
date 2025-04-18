package com.proyect.masterdata.helpers;

import com.proyect.masterdata.repository.OrderStateRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class OrderStateIdResolver {
    private final OrderStateRepository orderStateRepository;

    private final Map<String, UUID> orderStateCache = new HashMap<>();

    public List<UUID> resolveOrderStateIds(List<String> orderStateNames) {
        if (orderStateNames == null || orderStateNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = orderStateNames.stream()
                .map(String::toUpperCase)
                .toList();

        List<String> missingNames = upperNames.stream()
                .filter(name -> !orderStateCache.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            orderStateRepository.findByNameIn(missingNames)
                    .forEach(state ->
                            orderStateCache.put(state.getName().toUpperCase(), state.getId())
                    );
        }

        return upperNames.stream()
                .map(orderStateCache::get)
                .filter(Objects::nonNull)
                .toList();
    }
}

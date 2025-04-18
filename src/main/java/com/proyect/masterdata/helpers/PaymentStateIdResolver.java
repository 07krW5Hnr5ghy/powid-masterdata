package com.proyect.masterdata.helpers;

import com.proyect.masterdata.repository.OrderPaymentStateRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@RequiredArgsConstructor
@Component
public class PaymentStateIdResolver {

    private final OrderPaymentStateRepository orderPaymentStateRepository;

    private final Map<String, UUID> cache = new HashMap<>();

    public UUID resolve(String name) {
        if (name == null) return null;

        String upperName = name.toUpperCase();

        return cache.computeIfAbsent(upperName, key -> {
            var entity = orderPaymentStateRepository.findByName(key);
            return entity != null ? entity.getId() : null;
        });
    }
}

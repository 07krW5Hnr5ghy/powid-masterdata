package com.proyect.masterdata.helpers;

import com.proyect.masterdata.repository.SaleChannelRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.*;

@RequiredArgsConstructor
@Component
public class SaleChannelIdResolver {
    private final SaleChannelRepository saleChannelRepository;

    private final Map<String, UUID> saleChannelCache = new HashMap<>();

    public List<UUID> resolveSaleChannelIds(List<String> saleChannelNames) {
        if (saleChannelNames == null || saleChannelNames.isEmpty()) {
            return Collections.emptyList();
        }

        List<String> upperNames = saleChannelNames.stream()
                .map(String::toUpperCase)
                .toList();

        List<String> missingNames = upperNames.stream()
                .filter(name -> !saleChannelCache.containsKey(name))
                .toList();

        if (!missingNames.isEmpty()) {
            saleChannelRepository.findByNameIn(missingNames)
                    .forEach(channel ->
                            saleChannelCache.put(channel.getName().toUpperCase(), channel.getId())
                    );
        }

        return upperNames.stream()
                .map(saleChannelCache::get)
                .filter(Objects::nonNull)
                .toList();
    }
}

package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStockReplenishmentItem;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockReplenishmentItemImpl implements IStockReplenishmentItem {
    private final UserRepository userRepository;
    @Override
    public StockReplenishmentItem save(Ordering ordering, RequestStockReplenishmentItem requestStockReplenishmentItem, User user) throws InternalErrorExceptions, BadRequestExceptions {
        return null;
    }
}

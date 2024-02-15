package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockReplenishmentItem {
    public StockReplenishmentItem save(Ordering ordering, RequestStockReplenishmentItem requestStockReplenishmentItem, User user) throws InternalErrorExceptions, BadRequestExceptions;
}

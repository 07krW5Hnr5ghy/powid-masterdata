package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReplenishmentItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStockReplenishmentItem {
    StockReplenishmentItem save(OrderItem orderItem, RequestStockReplenishmentItem requestStockReplenishmentItem, User user, StockReplenishment stockReplenishment) throws InternalErrorExceptions, BadRequestExceptions;
    Page<StockReplenishmentItemDTO> list(String user,Long orderId,String productSku,String sort,String sortColumn,Integer pageNumber,Integer pageSize);
    List<StockReplenishmentItemDTO> listStockReplenishmentItem(String user) throws BadRequestExceptions,InternalErrorExceptions;
    List<StockReplenishmentItemDTO> listStockReplenishmentItemFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}

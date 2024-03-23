package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.dto.StockReplenishmentDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStockReplenishment {
    ResponseSuccess save(Long orderId, List<RequestStockReplenishmentItem> requestStockReplenishmentItems, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    Page<StockReplenishmentDTO> list(String user,Long orderId,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    List<StockReplenishmentDTO> listStockReplenishment(String user) throws BadRequestExceptions,InternalErrorExceptions;
    List<StockReplenishmentDTO> listStockReplenishmentFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}

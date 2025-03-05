package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.domain.WarehouseOutputItem;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.UUID;

public interface IWarehouseOutputItem {
    WarehouseOutputItem save(RequestWarehouseOutputItem requestWarehouseOutputItem, WarehouseOutput warehouseOutput, User user) throws BadRequestExceptions, InternalErrorExceptions;
}

package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IShipmentItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("shipment-item")
@AllArgsConstructor
public class ShipmentItemController {
    private final IShipmentItem iShipmentItem;
    @GetMapping()
    public ResponseEntity<Page<ShipmentItemDTO>> list(
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<ShipmentItemDTO> result = iShipmentItem.list(serial, user, warehouse, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}

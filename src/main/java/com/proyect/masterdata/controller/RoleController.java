package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.RoleDTO;
import com.proyect.masterdata.dto.request.RequestAccessesToRole;
import com.proyect.masterdata.dto.request.RequestRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IRole;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/role")
@AllArgsConstructor
public class RoleController {

    private IRole iRole;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iRole.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/roles", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iRole.saveAll(names, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<RoleDTO> update(
            @RequestBody() RequestRole requestUserRole) throws BadRequestExceptions {
        RoleDTO result = iRole.update(requestUserRole);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iRole.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<RoleDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<RoleDTO> result = iRole.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/status-false")
    public ResponseEntity<Page<RoleDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<RoleDTO> result = iRole.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}

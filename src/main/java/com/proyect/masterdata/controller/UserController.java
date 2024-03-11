package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.UserQueryDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUser;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("user")
@AllArgsConstructor
public class UserController {
    private IUser iUser;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:USER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestUser requestUser) throws BadRequestExceptions {
        ResponseSuccess result = iUser.save(requestUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_PUT')")
    public ResponseEntity<UserDTO> update(
            @RequestBody() RequestUserSave requestUserSave,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        UserDTO result = iUser.update(requestUserSave, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("username") String username,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iUser.delete(username,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:USER_GET')")
    public ResponseEntity<Page<UserQueryDTO>> list(
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "clientRuc", required = true) String clientRuc,
            @RequestParam(value = "dni", required = false) String dni,
            @RequestParam(value = "email",required = false) String email,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<UserQueryDTO> result = iUser.list(user, clientRuc, dni, email, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:USER_GET')")
    public ResponseEntity<Page<UserQueryDTO>> listFalse(
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "clientRuc", required = true) String clientRuc,
            @RequestParam(value = "dni", required = false) String dni,
            @RequestParam(value = "email",required = false) String email,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<UserQueryDTO> result = iUser.listFalse(user, clientRuc, dni, email, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping("activate")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("username") String username,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseSuccess result = iUser.activate(username,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}

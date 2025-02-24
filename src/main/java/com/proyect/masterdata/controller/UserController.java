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

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("user")
@AllArgsConstructor
public class UserController {
    private IUser iUser;
    @PostMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:USER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestUser requestUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iUser.saveAsync(requestUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_PUT')")
    public ResponseEntity<UserDTO> update(
            @RequestBody() RequestUserSave requestUserSave,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<UserDTO> result = iUser.update(requestUserSave, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("username") String username,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iUser.delete(username,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:USER_GET')")
    public ResponseEntity<Page<UserQueryDTO>> list(
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "names", required = false) List<String> names,
            @RequestParam(value = "usernames",required = false) List<String> usernames,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<UserQueryDTO>> result = iUser.list(user, names,usernames,sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:USER_GET')")
    public ResponseEntity<Page<UserQueryDTO>> listFalse(
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "names", required = false) List<String> names,
            @RequestParam(value = "usernames",required = false) List<String> usernames,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<UserQueryDTO>> result = iUser.listFalse(user, names,usernames,sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PutMapping("activate")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("username") String username,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iUser.activate(username,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<UserQueryDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, InterruptedException, ExecutionException {
        CompletableFuture<List<UserQueryDTO>> result = iUser.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}

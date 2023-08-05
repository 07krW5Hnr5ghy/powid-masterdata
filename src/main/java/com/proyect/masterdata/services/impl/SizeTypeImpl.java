package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SizeTypeMapper;
import com.proyect.masterdata.repository.SizeTypeRepository;
import com.proyect.masterdata.services.ISizeType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SizeTypeImpl implements ISizeType {
    private final SizeTypeRepository sizeTypeRepository;
    private final SizeTypeMapper sizeTypeMapper;

    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            sizeTypeRepository.save(sizeTypeMapper.sizeTypeToName(name.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
        try {
            sizeTypeRepository.saveAll(sizeTypeMapper.listSizeTypeToListName(
                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public SizeTypeDTO update(RequestSizeType requestSizeType) throws BadRequestExceptions {
        try {
            requestSizeType.setName(requestSizeType.getName().toUpperCase());
            SizeType updatedSizeType = sizeTypeMapper.requestSizeTypeToSizeType(requestSizeType);
            updatedSizeType.setDateRegistration(new Date(System.currentTimeMillis()));
            SizeType sizeType = sizeTypeRepository.save(updatedSizeType);
            return sizeTypeMapper.sizeTypeToSizeTypeDTO(sizeType);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            sizeTypeRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            sizeTypeRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<SizeTypeDTO> list() throws BadRequestExceptions{
        try {
            return sizeTypeMapper.listSizeTypeToListSizeTypeDTO(sizeTypeRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SizeTypeDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return sizeTypeMapper.sizeTypeToSizeTypeDTO(sizeTypeRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SizeTypeDTO findByName(String name) throws BadRequestExceptions{
        try {
            return sizeTypeMapper.sizeTypeToSizeTypeDTO(sizeTypeRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}

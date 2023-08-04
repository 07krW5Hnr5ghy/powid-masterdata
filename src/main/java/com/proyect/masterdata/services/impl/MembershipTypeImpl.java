package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.LogEvent;
import com.proyect.masterdata.domain.MembershipType;
import com.proyect.masterdata.dto.LogEventDTO;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.MembershipTypeDTO;
import com.proyect.masterdata.dto.request.RequestLogEvent;
import com.proyect.masterdata.dto.request.RequestMembershipType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.MembershipTypeMapper;
import com.proyect.masterdata.repository.MembershipTypeRepository;
import com.proyect.masterdata.services.IMasterList;
import com.proyect.masterdata.services.IMembershipType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class MembershipTypeImpl implements IMembershipType {
    private final MembershipTypeRepository membershipTypeRepository;
    private final MembershipTypeMapper membershipTypeMapper;

    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            membershipTypeRepository.save(membershipTypeMapper.membershipTypeToName(name.toUpperCase()));
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
            membershipTypeRepository.saveAll(membershipTypeMapper.listMembershipTypeToListName(
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
    public MembershipTypeDTO update(RequestMembershipType requestMembershipType) throws BadRequestExceptions {
        try {
            requestMembershipType.setName(requestMembershipType.getName().toUpperCase());
            MembershipType membershipType = membershipTypeRepository.save(membershipTypeMapper.requestMembershipTypeToMembershipType(requestMembershipType));
            return membershipTypeMapper.membershipTypeToMembershipTypeDTO(membershipType);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            membershipTypeRepository.deleteById(code);
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
            membershipTypeRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<MembershipTypeDTO> list() throws BadRequestExceptions{
        try {
            return membershipTypeMapper.listMembershipTypeToListMembershipTypeDTO(membershipTypeRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public MembershipTypeDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return membershipTypeMapper.membershipTypeToMembershipTypeDTO(membershipTypeRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public MembershipTypeDTO findByName(String name) throws BadRequestExceptions{
        try {
            return membershipTypeMapper.membershipTypeToMembershipTypeDTO(membershipTypeRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}

package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Courier;
import com.proyect.masterdata.domain.DeliveryCompany;
import com.proyect.masterdata.repository.DeliveryCompanyRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class DeliveryCompanyRepositoryCustomImpl implements DeliveryCompanyRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<DeliveryCompany> searchForDeliveryCompany(
            UUID clientId, 
            String name, 
            OffsetDateTime registrationStartDate, 
            OffsetDateTime registrationEndDate, 
            OffsetDateTime updateStartDate, 
            OffsetDateTime updateEndDate, 
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize, 
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<DeliveryCompany> criteriaQuery = criteriaBuilder.createQuery(DeliveryCompany.class);
        Root<DeliveryCompany> itemRoot = criteriaQuery.from(DeliveryCompany.class);
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
                clientId,
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot
        );
        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){

            List<Order> deliveryCompanyList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                deliveryCompanyList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                deliveryCompanyList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(deliveryCompanyList);
        }else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<DeliveryCompany> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getDeliveryCompanyCount(
                clientId,
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    public List<Predicate> predicate(
            UUID clientId,
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<DeliveryCompany> itemRoot
    ){
        
        List<Predicate> conditions = new ArrayList<>();
        
        if(name!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("name")),"%"+name.toUpperCase()+"%"));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if(Boolean.TRUE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(Boolean.FALSE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }
    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<DeliveryCompany> itemRoot) {

        List<Order> deliveryCompanyList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("name")) {
            deliveryCompanyList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            deliveryCompanyList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryCompanyList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryCompanyList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryCompanyList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryCompanyList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return deliveryCompanyList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<DeliveryCompany> itemRoot) {

        List<Order> deliveryCompanyList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("name")) {
            deliveryCompanyList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            deliveryCompanyList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryCompanyList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryCompanyList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryCompanyList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryCompanyList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return deliveryCompanyList;
    }

    private Long getDeliveryCompanyCount(
            UUID clientId,
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status
    ){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<DeliveryCompany> itemRoot = criteriaQuery.from(DeliveryCompany.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot
        );
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}

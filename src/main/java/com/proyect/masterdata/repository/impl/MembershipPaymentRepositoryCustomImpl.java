package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.repository.MembershipPaymentRepositoryCustom;
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
public class MembershipPaymentRepositoryCustomImpl implements MembershipPaymentRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<MembershipPayment> searchForMembershipPayment(
            UUID clientId,
            Double grossAmount,
            Double netAmount,
            Double paymentGatewayFee,
            Double taxAmount,
            UUID paymentGatewayId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MembershipPayment> criteriaQuery = criteriaBuilder.createQuery(MembershipPayment.class);
        Root<MembershipPayment> itemRoot = criteriaQuery.from(MembershipPayment.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                clientId,
                grossAmount,
                netAmount,
                paymentGatewayFee,
                taxAmount,
                paymentGatewayId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> paymentList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                paymentList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                paymentList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(paymentList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<MembershipPayment> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                clientId,
                grossAmount,
                netAmount,
                paymentGatewayFee,
                taxAmount,
                paymentGatewayId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            UUID clientId,
            Double grossAmount,
            Double netAmount,
            Double paymentGatewayFee,
            Double taxAmount,
            UUID paymentGatewayId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<MembershipPayment> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("clientId"), clientId)));
        }

        if (grossAmount != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("grossAmount"), grossAmount)));
        }

        if (netAmount != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("netAmount"), netAmount)));
        }

        if (paymentGatewayFee != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("paymentGatewayFee"), paymentGatewayFee)));
        }

        if (taxAmount != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("taxAmount"), taxAmount)));
        }

        if (paymentGatewayId != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("paymentGatewayId"), paymentGatewayId)));
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

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<MembershipPayment> itemRoot) {
        List<Order> paymentList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("clientId")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("grossAmount")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("grossAmount")));
        }
        if (sortColumn.equalsIgnoreCase("netAmount")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("netAmount")));
        }
        if (sortColumn.equalsIgnoreCase("paymentGatewayFee")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("paymentGatewayFee")));
        }
        if (sortColumn.equalsIgnoreCase("taxAmount")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("taxAmount")));
        }
        if (sortColumn.equalsIgnoreCase("paymentGatewayId")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("paymentGatewayId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            paymentList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return paymentList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<MembershipPayment> itemRoot) {
        List<Order> paymentList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("clientId")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("grossAmount")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("grossAmount")));
        }
        if (sortColumn.equalsIgnoreCase("netAmount")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("netAmount")));
        }
        if (sortColumn.equalsIgnoreCase("paymentGatewayFee")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("paymentGatewayFee")));
        }
        if (sortColumn.equalsIgnoreCase("taxAmount")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("taxAmount")));
        }
        if (sortColumn.equalsIgnoreCase("paymentGatewayId")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("paymentGatewayId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            paymentList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return paymentList;
    }

    private long getOrderCount(
            UUID clientId,
            Double grossAmount,
            Double netAmount,
            Double paymentGatewayFee,
            Double taxAmount,
            UUID paymentGatewayId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<MembershipPayment> itemRoot = criteriaQuery.from(MembershipPayment.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                grossAmount,
                netAmount,
                paymentGatewayFee,
                taxAmount,
                paymentGatewayId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
